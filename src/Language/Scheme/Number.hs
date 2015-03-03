module Language.Scheme.Number
( SchemeReal(..)
, SchemeComplex(..)
, toExact
, toInexact
, numberEq
) where

import Data.Complex

-- | A real number, which might be exact or inexact.
data SchemeReal
  = Exact Rational
  | Inexact Double
  deriving (Show, Read)

-- | This implements Scheme's mathematical equality (@=@) operator.
instance Eq SchemeReal where
  Inexact x == Inexact y = x == y
  x == y = toRational x == toRational y

instance Ord SchemeReal where
  compare (Inexact x) (Inexact y) = compare x y
  compare x y = compare (toRational x) (toRational y)

toDouble :: SchemeReal -> Double
toDouble (Exact x) = realToFrac x
toDouble (Inexact x) = x

instance Num SchemeReal where
  Exact x + Exact y = Exact $ x + y
  x + y = Inexact $ toDouble x + toDouble y
  Exact x * Exact y = Exact $ x * y
  x * y = Inexact $ toDouble x * toDouble y
  negate (Exact x) = Exact $ negate x
  negate (Inexact y) = Inexact $ negate y
  abs (Exact x) = Exact $ abs x
  abs (Inexact x) = Inexact $ abs x
  signum (Exact x) = Exact $ signum x
  signum (Inexact x) = Inexact $ signum x
  fromInteger x = Exact $ fromInteger x

instance Real SchemeReal where
  toRational (Exact x) = x
  toRational (Inexact x) = toRational x

instance Fractional SchemeReal where
  recip (Exact x) = Exact $ recip x
  recip (Inexact x) = Inexact $ recip x
  fromRational = Exact

instance Floating SchemeReal where
  pi = Inexact pi
  --
  exp = wrap1 exp
  sqrt = wrap1 sqrt -- unnecessary
  log = wrap1 log
  --
  x ** y = Inexact $ toDouble x ** toDouble y -- unnecessary
  logBase x y = Inexact $ logBase (toDouble x) (toDouble y) -- unnecessary
  --
  sin = wrap1 sin
  tan = wrap1 tan -- unnecessary
  cos = wrap1 cos
  --
  sinh = wrap1 sinh
  tanh = wrap1 tanh -- unnecessary
  cosh = wrap1 cosh
  --
  asin = wrap1 asin
  atan = wrap1 atan
  acos = wrap1 acos
  --
  asinh = wrap1 asinh
  atanh = wrap1 atanh
  acosh = wrap1 acosh

wrap1 :: (Double -> Double) -> SchemeReal -> SchemeReal
wrap1 f = Inexact . f . toDouble

instance RealFrac SchemeReal where
  properFraction (Exact   x) = case properFraction x of (y, z) -> (y, Exact   z)
  properFraction (Inexact x) = case properFraction x of (y, z) -> (y, Inexact z)

-- | A type that implements the whole Scheme numerical tower system.
-- Both the real and imaginary parts can each be exact or inexact.
data SchemeComplex = SchemeReal :+\ SchemeReal
  deriving (Eq, Show, Read)
infix 6 :+\

toComplex :: SchemeComplex -> Complex Double
toComplex (x :+\ y) = toDouble x :+ toDouble y

fromComplex :: Complex Double -> SchemeComplex
fromComplex (x :+ y) = Inexact x :+\ Inexact y

instance Num SchemeComplex where
  (a :+\ b) + (c :+\ d) = (a + c) :+\ (b + d)
  (a :+\ b) * (c :+\ d) = (a * c - b * d) :+\ (a * d + b * c)
  abs (a :+\ Exact 0) = abs a :+\ Exact 0
  abs (Exact 0 :+\ b) = abs b :+\ Exact 0
  abs sc = Inexact (realPart $ abs $ toComplex sc) :+\ Exact 0
  negate (a :+\ b) = negate a :+\ negate b
  signum (a :+\ Exact 0) = signum a :+\ Exact 0
  signum (Exact 0 :+\ b) = Exact 0 :+\ signum b
  signum sc = fromComplex . signum . toComplex $ sc
  fromInteger x = fromInteger x :+\ Exact 0

instance Fractional SchemeComplex where
  fromRational x = Exact x :+\ Exact 0
  recip (a :+\ Exact 0) = recip a :+\ Exact 0
  recip (Exact 0 :+\ b) = Exact 0 :+\ negate (recip b)
  recip sc = fromComplex . recip . toComplex $ sc

instance Floating SchemeComplex where
  pi = pi :+\ Exact 0
  --
  exp = wrapc exp
  sqrt = wrapc sqrt -- unnecessary
  log = wrapc log
  --
  sin = wrapc sin
  tan = wrapc tan -- unnecessary
  cos = wrapc cos
  --
  sinh = wrapc sinh
  tanh = wrapc tanh -- unnecessary
  cosh = wrapc cosh
  --
  asin = wrapc asin
  atan = wrapc atan
  acos = wrapc acos
  --
  asinh = wrapc asinh
  atanh = wrapc atanh
  acosh = wrapc acosh

wrapc :: (Complex Double -> Complex Double) -> SchemeComplex -> SchemeComplex
wrapc f = fromComplex . f . toComplex

-- | Makes the real and imaginary parts exact.
toExact :: SchemeComplex -> SchemeComplex
toExact (x :+\ y) = Exact (toRational x) :+\ Exact (toRational y)

-- | Makes the real and imaginary parts inexact, except: an exact zero imaginary
-- part will be preserved.
toInexact :: SchemeComplex -> SchemeComplex
toInexact (x :+\ z@(Exact 0)) = Inexact (toDouble x) :+\ z
toInexact (x :+\ y) = Inexact (toDouble x) :+\ Inexact (toDouble y)

-- | Implements Scheme's @eq@ function on numbers. Exact numbers and inexact
-- numbers are not equal to each other by this function.
numberEq :: SchemeComplex -> SchemeComplex -> Bool
numberEq (a :+\ b) (c :+\ d) = realEq a c && realEq b d where
  realEq (Exact x) (Exact y) = x == y
  realEq (Inexact x) (Inexact y) = x == y
  realEq _ _ = False
