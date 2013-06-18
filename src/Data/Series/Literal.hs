{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Series.Literal
  ( Literal(..)
  , Literate(..)
  , int
  , integer
  , rational
  , double
  , string
  , day
  , Lit(..)
  , lit1
  , lit2
  ) where

import Control.Lens
import Data.Time
import Data.Typeable

data Literal a where
  Int      :: Int -> Literal Int
  Double   :: Double -> Literal Double
  String   :: String -> Literal String
  Integer  :: Integer -> Literal Integer
  Rational :: Rational -> Literal Rational
  Day      :: Day -> Literal Day

lit1 :: (a -> a) -> Literal a -> Literal a
lit1 f (Int a) = Int (f a)
lit1 f (Double a) = Double (f a)
lit1 f (String a) = String (f a)
lit1 f (Integer a) = Integer (f a)
lit1 f (Rational a) = Rational (f a)
lit1 f (Day a) = Day (f a)

lit2 :: (a -> a -> a) -> Literal a -> Literal a -> Literal a
lit2 f (Int a) (Int b) = Int (f a b)
lit2 f (Double a) (Double b) = Double (f a b)
lit2 f (String a) (String b) = String (f a b)
lit2 f (Integer a) (Integer b) = Integer (f a b)
lit2 f (Rational a) (Rational b) = Rational (f a b)
lit2 f (Day a) (Day b) = Day (f a b)
lit2 _ _ _ = error "Really, GHC. Really?"

class Typeable a => Lit a where
  lit :: Literate t => a -> t a

instance Lit Int where
  lit = int

instance Lit Integer where
  lit = integer

instance Lit Double where
  lit = double

instance Lit Rational where
  lit = rational

instance Lit String where
  lit = string

instance Lit Day where
  lit = day

class Literate t where
  _Literal :: Lit a => Prism' (t a) (Literal a)

instance Literate Literal where
  _Literal = id

int :: Literate t => Int -> t Int
int a = _Literal # Int a

integer :: Literate t => Integer -> t Integer
integer a = _Literal # Integer a

rational :: Literate t => Rational -> t Rational
rational a = _Literal # Rational a

double :: Literate t => Double -> t Double
double a = _Literal # Double a

string :: Literate t => String -> t String
string a = _Literal # String a

day :: Literate t => Day -> t Day
day a = _Literal # Day a
