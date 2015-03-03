{-# LANGUAGE RankNTypes #-}
module Language.Scheme.Base where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Error
import Control.Monad.Trans.Class
import Control.Monad.ST
import Data.STRef
import Data.Array.IArray
import Data.Array.ST
import Control.Applicative
import qualified Data.HashTable.ST.Basic as H
import Language.Scheme.Number
import qualified Data.Text as T
import Data.Traversable (traverse)

type Scheme s = ErrorT String (ReaderT (Context s) (ST s))

liftST :: ST s a -> Scheme s a
liftST = lift . lift

liftReader :: ReaderT (Context s) (ST s) a -> Scheme s a
liftReader = lift

crash :: String -> Scheme s a
crash = throwError

-- | A symbol is a pointer-like value, which can be dereferenced to yield a
-- string. But, because it is a pointer, testing for equality is O(1).
type Symbol s = STRef s String

data Context s = Context
  { contextNames :: H.HashTable s String (Var s)
  , contextSymbols :: H.HashTable s String (Symbol s) }

-- | TODO: multiple return values
type Proc s = [Val s] -> Scheme s (Val s)

emptyContext :: ST s (Context s)
emptyContext = liftA2 Context H.new H.new

runScheme
  :: (forall s. ST s (Context s)) -> (forall s. Scheme s a) -> Either String a
runScheme ctxt prog = runST $ ctxt >>= runReaderT (runErrorT prog)

-- | A Scheme value, connected to a state thread.
data Val s
  = Pure Pure
  | Symbol (Symbol s)
  | Object (STRef s (Obj s))
  -- ^ The STRef pointing to an Obj should never be modified once initialized;
  -- its only purpose is to provide \"pointer-based\" equality among objects.
  deriving (Eq)

-- | An atomic data type which does not depend on a state thread.
-- The constructors marked Pure aren't actual Scheme values; they are only used
-- for serialization.
data Pure
  = Bool Bool
  | Num SchemeComplex
  | Char Char
  | Null
  | PureSymbol String
  | PurePair Pure Pure
  | PureString T.Text
  | PureVector (Array Int Pure)
  | PureProc -- ^ opaque value
  deriving (Eq, Show, Read)

data Obj s
  = IPair (Val s) (Val s)
  | IString T.Text
  | IVector (Array Int (Val s))
  | Pair (Var s) (Var s)
  | String (STUArray s Int Char)
  | Vector (STArray s Int (Val s))
  | Proc (Proc s)

type Var s = STRef s (Val s)

deref :: STRef s a -> Scheme s a
deref = liftST . readSTRef

-- | Performs a single binding for the duration of an expression.
bind :: String -> Var s -> Scheme s a -> Scheme s a
bind name var expr = do
  names <- liftReader $ asks contextNames
  old <- liftST $ H.lookup names name
  liftST $ H.insert names name var
  result <- expr
  liftST $ case old of
    Nothing -> H.delete names name
    Just v -> H.insert names name v
  return result

-- | Returns the (possibly already existing) symbol pointer.
symbol :: String -> Scheme s (Symbol s)
symbol str = do
  syms <- liftReader $ asks contextSymbols
  cur <- liftST $ H.lookup syms str
  case cur of
    Nothing -> do
      ref <- liftST $ newSTRef str
      liftST $ H.insert syms str ref
      return ref
    Just ref -> return ref

-- | Creates a pointer to an object. This should then be the sole reference to
-- the object, because it's what @eq?@ uses to determine object equality.
new :: Obj s -> Scheme s (Val s)
new = fmap Object . liftST . newSTRef

-- | Creates a new variable, a mutable memory location.
box :: Val s -> Scheme s (Var s)
box = liftST . newSTRef

-- | Extracts the current values from a mutable or immutable pair.
getPair :: Val s -> Scheme s (Maybe (Val s, Val s))
getPair (Pure (PurePair x y)) = return $ Just (Pure x, Pure y)
getPair (Object tag) = deref tag >>= \obj -> case obj of
  IPair x y -> return $ Just (x, y)
  Pair x y -> fmap Just $ liftA2 (,) (deref x) (deref y)
  _ -> return Nothing
getPair _ = return Nothing

-- | Extracts a proper list.
getList :: Val s -> Scheme s (Maybe [Val s])
getList (Pure Null) = return $ Just []
getList v = getPair v >>= \mb -> case mb of
  Nothing -> return Nothing
  Just (x, y) -> fmap (x :) <$> getList y

-- | Allocates a new mutable list holding the given values.
list :: [Val s] -> Scheme s (Val s)
list [] = return $ Pure Null
list (x:xs) = do
  b <- box x
  bs <- list xs >>= box
  new $ Pair b bs

-- | Allocates a new immutable list holding the given values.
listI :: [Val s] -> Scheme s (Val s)
listI [] = return $ Pure Null
listI (x:xs) = listI xs >>= new . IPair x

eval :: Val s -> Scheme s (Val s)
eval (Symbol s) = do
  str <- deref s
  names <- liftReader $ asks contextNames
  liftST (H.lookup names str) >>=
    maybe (crash $ "Undefined identifier: " ++ str) deref
eval x = getPair x >>= \mp -> case mp of
  Nothing -> return x
  Just (car, cdr) -> case car of
    Symbol sym -> do
      str <- deref sym
      case str of
      
        "quote" -> getList cdr >>= \ml -> case ml of
          Just [q] -> return q
          _ -> crash "quote: invalid syntax, expected 1 argument"
          
        "if" -> getList cdr >>= \ml -> case ml of
          Just [b, t, f] -> eval b >>= \b' -> eval $
            if b' /= Pure (Bool False) then t else f
          Just [b, t] -> eval b >>= \b' ->
            if b' /= Pure (Bool False) then eval t else return $ Pure Null
          _ -> crash "if: invalid syntax, expected 2 or 3 arguments"
        
        "lambda" -> getList cdr >>= \ml -> case ml of
          Just (args : body) -> lambda args body >>= new . Proc
          _ -> crash "lambda: invalid syntax, expected at least 1 argument"

        "eval" -> getList cdr >>= \ml -> case ml of
          Just [v] -> eval v >>= eval
          _ -> crash "eval: invalid syntax, expected 1 argument"

        "let" -> getList cdr >>= \ml -> case ml of
          Just (bound : body) -> do
            bindings <- getList bound >>=
              maybe (crash "let: invalid syntax, bindings must be a list")
              (mapM getList2)
            names <- list $ map fst bindings
            proc <- lambda names body
            mapM eval (map snd bindings) >>= proc
            where getList2 v = getList v >>= \xs -> case xs of
                    Just [name, val] -> return (name, val)
                    _ -> crash "let: invalid syntax, binding must be (name val)"
          _ -> crash "let: invalid syntax, expected at least 1 argument"
        
        _ -> apply car cdr
        
    _ -> apply car cdr

-- | Given the args list and body of a lambda expression, creates a new
-- procedure.
lambda :: Val s -> [Val s] -> Scheme s (Proc s)
lambda formals body = do
  (forms, ftail) <- parseFormals formals
  let proc = case body of
        []    -> return $ Pure Null
        _ : _ -> foldr1 (>>) $ map eval body
  return $ \acts -> bindFormals forms ftail acts proc

bindFormals :: [String] -> Maybe String -> [Val s] -> Scheme s a -> Scheme s a
bindFormals [] Nothing      []   proc = proc
bindFormals [] (Just ftail) acts proc = do
  atail <- listI acts >>= box
  bind ftail atail proc
bindFormals (f:fs) ftail (x:xs) proc = box x >>= \x' ->
  bind f x' $ bindFormals fs ftail xs proc
bindFormals _ _ _ _ = crash "bindFormals: improper arguments for a procedure" 

-- | Given the formal parameters list of a lambda, returns the list of formals,
-- as well as the optional \"slurp\" name for the remaining actuals.
parseFormals :: Val s -> Scheme s ([String], Maybe String)
parseFormals v = case v of
  Pure Null -> return ([], Nothing)
  Symbol sym -> deref sym >>= \str -> return ([], Just str)
  _ -> getPair v >>= \p -> case p of
    Just (car, cdr) -> case car of
      Symbol sym -> do
        str <- deref sym
        (xs, t) <- parseFormals cdr
        return (str : xs, t)
      _ -> crash "parseFormals: non-identifier in formal parameters list"
    Nothing -> crash "parseFormals: invalid formal parameters list"

-- | Function application, given the car (function) and cdr (list of args).
apply :: Val s -> Val s -> Scheme s (Val s)
apply car cdr = eval car >>= \car' -> case car' of
  Object t -> deref t >>= \o -> case o of
    Proc f -> getList cdr >>= \ml -> case ml of
      Just xs -> mapM eval xs >>= f
      _ -> crash "function application: cdr isn't a proper list"
    _ -> crash "function application: car isn't a function"
  _ -> crash "function application: car isn't a function"

getNumber :: Val s -> Maybe SchemeComplex
getNumber (Pure (Num n)) = Just n
getNumber _ = Nothing

list' :: [Pure] -> Pure
list' = foldr PurePair Null

purify :: Val s -> Scheme s Pure
purify v = case v of
  Pure p -> return p
  Symbol sym -> PureSymbol <$> deref sym
  Object t -> deref t >>= \o -> case o of
    IPair x y -> liftA2 PurePair (purify x) (purify y)
    IString txt -> return $ PureString txt
    IVector ary -> PureVector <$> traverse purify ary
    Pair x y -> liftA2 PurePair (deref x >>= purify) (deref y >>= purify)
    String ary -> PureString . T.pack <$> liftST (getElems ary)
    Vector ary -> fmap PureVector $ liftST (freeze ary) >>= traverse purify
    Proc _ -> return PureProc

-- | Allocates a Pure value as pointer-based, but immutable, Scheme values.
impurify :: Pure -> Scheme s (Val s)
impurify v = case v of
  PureSymbol str -> Symbol <$> symbol str
  PurePair x y -> do
    x' <- impurify x
    y' <- impurify y
    new $ IPair x' y'
  PureString txt -> new $ IString txt
  PureVector ary -> traverse impurify ary >>= new . IVector
  PureProc -> crash "impurify: can't unmarshall procedure"
  _ -> return $ Pure v
