module Language.Scheme.Prelude
( baseContext
) where

import Language.Scheme.Base
import Language.Scheme.Number

import Control.Monad (forM_)
import Control.Monad.ST
import Data.STRef
import qualified Data.HashTable.ST.Basic as H
import Data.List (foldl')

argerr :: String -> String -> String -> Scheme s a
argerr fn ex got = crash $
  fn ++ ": expected " ++ ex ++ " argument(s), got " ++ got

baseContext :: ST s (Context s)
baseContext = do
  ctxt <- emptyContext
  forM_ base $ \(str, f) -> do
    val <- fmap Object $ newSTRef (Proc f)
    ref <- newSTRef val
    H.insert (contextNames ctxt) str ref
  return ctxt

base :: [(String, Proc s)]
base =
  [ ("+", plus)
  , ("*", times)
  , ("-", minus)
  , ("/", divide)
  , ("boolean?", isBoolean)
  , ("symbol?", isSymbol)
  , ("char?", isChar)
  , ("null?", isNull)
  , ("vector?", isVector)
  , ("pair?", isPair)
  , ("string?", isString)
  , ("proc?", isProc)
  , ("number?", isNumber)
  , ("complex?", isComplex)
  , ("car", car)
  , ("cdr", cdr)
  , ("exact", exact)
  , ("inexact", inexact)
  , ("list", list)
  , ("cons", cons)
  ]

plus :: Proc s
plus args = case mapM getNumber args of
  Just ns -> return $ Pure $ Num $ sum ns
  Nothing -> crash "+: received non-number argument"

times :: Proc s
times args = case mapM getNumber args of
  Just ns -> return $ Pure $ Num $ product ns
  Nothing -> crash "*: received non-number argument"

minus :: Proc s
minus args = case mapM getNumber args of
  Just []     -> argerr "-" "at least 1" "0"
  Just [n]    -> return $ Pure $ Num $ negate n
  Just (n:ns) -> return $ Pure $ Num $ foldl' (-) n ns
  Nothing     -> crash "-: received non-number argument"

divide :: Proc s
divide args = case mapM getNumber args of
  Just []     -> argerr "/" "at least 1" "0"
  Just [n]    -> return $ Pure $ Num $ recip n
  Just (n:ns) -> return $ Pure $ Num $ foldl' (/) n ns
  Nothing     -> crash "/: received non-number argument"

isBoolean :: Proc s
isBoolean args = case args of
  [Pure (Bool _)] -> return $ Pure $ Bool True
  [_]             -> return $ Pure $ Bool False
  _               -> argerr "boolean?" "1" $ show $ length args

isSymbol :: Proc s
isSymbol args = case args of
  [Symbol _] -> return $ Pure $ Bool True
  [_]        -> return $ Pure $ Bool False
  _          -> argerr "symbol?" "1" $ show $ length args

isChar :: Proc s
isChar args = case args of
  [Pure (Char _)] -> return $ Pure $ Bool True
  [_]             -> return $ Pure $ Bool False
  _               -> argerr "char?" "1" $ show $ length args

isNull :: Proc s
isNull args = case args of
  [Pure Null] -> return $ Pure $ Bool True
  [_]         -> return $ Pure $ Bool False
  _           -> argerr "null?" "1" $ show $ length args

isVector :: Proc s
isVector args = case args of
  [Object t] -> deref t >>= \o -> case o of
    IVector _ -> return $ Pure $ Bool True
    Vector  _ -> return $ Pure $ Bool True
    _         -> return $ Pure $ Bool False
  [_]        -> return $ Pure $ Bool False
  _          -> argerr "vector?" "1" $ show $ length args

isPair :: Proc s
isPair args = case args of
  [Object t] -> deref t >>= \o -> case o of
    IPair _ _ -> return $ Pure $ Bool True
    Pair  _ _ -> return $ Pure $ Bool True
    _         -> return $ Pure $ Bool False
  [_]        -> return $ Pure $ Bool False
  _          -> argerr "pair?" "1" $ show $ length args

isString :: Proc s
isString args = case args of
  [Object t] -> deref t >>= \o -> case o of
    IString _ -> return $ Pure $ Bool True
    String  _ -> return $ Pure $ Bool True
    _         -> return $ Pure $ Bool False
  [_]        -> return $ Pure $ Bool False
  _          -> argerr "string?" "1" $ show $ length args

isProc :: Proc s
isProc args = case args of
  [Object t] -> deref t >>= \o -> case o of
    Proc _ -> return $ Pure $ Bool True
    _      -> return $ Pure $ Bool False
  [_]        -> return $ Pure $ Bool False
  _          -> argerr "proc?" "1" $ show $ length args

isNumber :: Proc s
isNumber args = case args of
  [Pure (Num _)] -> return $ Pure $ Bool True
  [_]            -> return $ Pure $ Bool False
  _              -> argerr "number?" "1" $ show $ length args

isComplex :: Proc s
isComplex args = case args of
  [Pure (Num _)] -> return $ Pure $ Bool True
  [_]            -> return $ Pure $ Bool False
  _              -> argerr "complex?" "1" $ show $ length args

car :: Proc s
car args = case args of
  [x] -> getPair x >>= maybe (crash "car: not a pair") (return . fst)
  _   -> argerr "car" "1" $ show $ length args

cdr :: Proc s
cdr args = case args of
  [x] -> getPair x >>= maybe (crash "cdr: not a pair") (return . snd)
  _   -> argerr "cdr" "1" $ show $ length args

cons :: Proc s
cons args = case args of
  [x, y] -> do
    x' <- box x
    y' <- box y
    new $ Pair x' y'
  _ -> argerr "cons" "2" $ show $ length args

exact :: Proc s
exact args = case args of
  [x] -> maybe (crash err) (return . Pure . Num . toExact) $ getNumber x
  _   -> argerr "exact" "1" $ show $ length args
  where err = "exact: not a number"

inexact :: Proc s
inexact args = case args of
  [x] -> maybe (crash err) (return . Pure . Num . toInexact) $ getNumber x
  _   -> argerr "inexact" "1" $ show $ length args
  where err = "inexact: not a number"
