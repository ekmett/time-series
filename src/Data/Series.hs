{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ParallelListComp #-}
module Data.Series where

import Control.Applicative
import Control.Lens
import Data.Foldable
import Data.Ratio
import Data.Time
import Data.Traversable

newtype Source = Source String
makePrisms ''Source

newtype Column = Column String
makePrisms ''Column

data Type a where
  INT      :: Type Int
  DOUBLE   :: Type Double
  STRING   :: Type String
  INTEGER  :: Type Integer
  RATIONAL :: Type Rational
  DAY      :: Type Day

class Typed a where
  infer :: t a -> Type a

data Literal a where
  Int      :: Int -> Literal Int
  Double   :: Double -> Literal Double
  String   :: String -> Literal String
  Integer  :: Integer -> Literal Integer
  Rational :: Rational -> Literal Rational
  Day      :: Day -> Literal Day

lift1 :: (a -> a) -> Literal a -> Literal a
lift1 f (Int a) = Int (f a)
lift1 f (Double a) = Double (f a)
lift1 f (String a) = String (f a)
lift1 f (Integer a) = Integer (f a)
lift1 f (Rational a) = Rational (f a)
lift1 f (Day a) = Day (f a)

lift2 :: (a -> a -> a) -> Literal a -> Literal a -> Literal a
lift2 f (Int a) (Int b) = Int (f a b)
lift2 f (Double a) (Double b) = Double (f a b)
lift2 f (String a) (String b) = String (f a b)
lift2 f (Integer a) (Integer b) = Integer (f a b)
lift2 f (Rational a) (Rational b) = Rational (f a b)
lift2 f (Day a) (Day b) = Day (f a b)


instance (Typed a, Num a) => Num (Literal a) where
  (+) = lift2 (+)
  (-) = lift2 (-)
  negate = lift1 negate
  abs = lift1 abs
  signum = lift1 signum
  (*) = lift2 (*)
  fromInteger = undefined -- TODO, use infer

instance Typed Int where
  infer _ = INT

instance Typed Double where
  infer _ = DOUBLE

instance Typed String where
  infer _ = STRING

instance Typed Integer where
  infer _ = INTEGER

instance Typed Rational where
  infer _ = RATIONAL

instance Typed Day where
  infer _ = DAY

class Literate t => Sorted t

class Sorted t => Timed t where
  type Delta :: (* -> *) -> *

class Literate t where
  _Literal :: Prism' (t a) (Literal a)

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

-- _Int :: Literate t => Prism (t Int) Int

class Lit a where
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

data Dir = Asc | Desc

inferL :: Literal a -> Type a
inferL Int{}      = INT
inferL Rational{} = RATIONAL
inferL Double{}   = DOUBLE
inferL Integer{}  = INTEGER
inferL Day{}      = DAY

inferF :: F t a -> Type a
inferF (Var a)         = infer a
inferF (Sliding _ f a) = inferF (f a)
inferF (By _ f a)      = inferF (f a)
inferF EMA{}           = DOUBLE
inferF (Literal a)     = inferL a
inferF (Integral a)    = inferF a
inferF Step{}          = INT
inferF (Every _ a)     = inferF a
inferF (Sum a)         = inferF a
inferF Monthly         = DAY
inferF Daily           = DAY
inferF (Median a)      = inferF a
inferF (First a)       = inferF a
inferF (Last a)        = inferF a
inferF (ArgMin _ b)    = inferF b
inferF (ArgMax _ b)    = inferF b
inferF (OrElse a _)    = inferF a
inferF (Delay _ _ a)   = inferF a
inferF (a :+ _)        = inferF a
inferF (a :- _)        = inferF a
inferF (Negate a)      = inferF a
inferF (a :* _)        = inferF a
inferF (Abs a)         = inferF a
inferF (Signum a)      = inferF a
inferF (a :/ _)        = inferF a
inferF (Recip a)       = inferF a

-- behavior model [(t,t,a)] -- starting time, ending time, value

data F :: (* -> *) -> * -> * where
  Var      :: Typed a               => t a -> F t a

  -- * Memory
  Sliding  :: Timed t               => Delta t -> (forall s. Sorted s => F s as -> F s b) -> F t as -> F t b
  By       ::                          F t a -> (forall s. F s bs -> F s c) -> F t bs -> F t c
  -- Sorting  ::                       [(F t a, Dir)] -> (forall s. Sorted s => F s bs -> F s c) -> F t bs -> F t c

  -- * Running Tallies
  EMA      :: Timed t               => Delta t -> Double -> F t Double -> F t Double
  Integral :: (Sorted t, Num a)     => F t a -> F t a
  Step     :: Sorted t              => F t a -> F t Int
  Delay    :: Timed t               => Delta t -> a -> F t a -> F t a
  FwdFill  :: Timed t               => Delta t -> F t a -> F t a
  BackFill :: Timed t               => Delta t -> F t a -> F t a

  -- * Normal
  Literal  ::                          Literal a -> F t a
  Every    ::                          F t b -> F t a -> F t a
  Monthly  :: Timed t               => F t Day
  Daily    :: Timed t               => F t Day
  OrElse   ::                          F t a -> F t a -> F t a
  (:+)     :: Num a                 => F t a -> F t a -> F t a
  (:-)     :: Num a                 => F t a -> F t a -> F t a
  Negate   :: Num a                 => F t a -> F t a
  (:*)     :: Num a                 => F t a -> F t a -> F t a
  Abs      :: Num a                 => F t a -> F t a
  Signum   :: Num a                 => F t a -> F t a
  (:/)     :: Fractional a          => F t a -> F t a -> F t a
  Recip    :: Fractional a          => F t a -> F t a

  -- * Passes
  Sum      :: Num a                 => F t a -> F t a
  Median   :: Fractional a          => F t a -> F t a
  First    :: Sorted t              => F t a -> F t a
  Last     :: Sorted t              => F t a -> F t a
  ArgMin   ::                          F t a -> F t b -> F t b
  ArgMax   ::                          F t a -> F t b -> F t b

instance (Lit a, Num a) => Num (F t a) where
  (+) = (:+)
  (-) = (:-)
  negate = Negate
  (*) = (:*)
  abs = Abs
  signum = Signum
  fromInteger = lit . fromInteger

instance (Lit a, Fractional a) => Fractional (F t a) where
  (/) = (:/)
  recip = Recip
  fromRational = lit . fromRational

instance Literate (F t) where
  _Literal = prism' Literal $ \a -> case a of
    Literal l -> Just l
    _         -> Nothing

replaceMissing :: Lit a => a -> F t a -> F t a
replaceMissing a p = p `OrElse` lit a

mad :: (Fractional a, Num a, Lit a, Ord a) => F t a -> F t a
mad a = median $ abs (a - median a)

minimal, maximal :: F t a -> F t a
minimal x = ArgMin x x
maximal x = ArgMax x x

-- convenience functions

orElse :: F t a -> F t a -> F t a
orElse = OrElse

median :: (Fractional a, Ord a) => F t a -> F t a
median = Median

sliding :: Timed t => Delta t -> (forall s. Sorted s => F s as -> F s b) -> F t as -> F t b
sliding = Sliding

by :: F t a -> (forall s. F s bs -> F s c) -> F t bs -> F t c
by = By

last  :: Sorted t => F t a -> F t a
last = Last

first :: Sorted t => F t a -> F t a
first = First

newtype Behavior a
  = Behavior { runBehavior :: [(Day,Day,a)] } deriving (Show,Read,Eq,Ord)
  -- | Given (Literal a)

instance Functor Behavior where
  fmap = fmapDefault

instance FunctorWithIndex Day Behavior where
  -- imap = imapDefault

instance Foldable Behavior where
  foldMap = foldMapDefault

instance FoldableWithIndex Day Behavior where
  -- ifoldMap = ifoldMapDefault

instance Traversable Behavior where
  traverse f (Behavior as) = Behavior <$> traverse g as where
    g (a,b,c) = (,,) a b <$> f c

instance TraversableWithIndex Day Behavior where
  itraverse f (Behavior as) = Behavior <$> traverse g as where
    g (a,b,c) = (,,) a b <$> f a c

instance Literate Behavior where
  -- _Literal :: Prism' (t a) (Literal a)

instance Sorted Behavior
instance Timed Behavior where

  -- type Delta Behavior = NominalDiffTime

eval :: F Behavior a -> Either (Literal a) (Behavior a)
eval (Var as) = Right as
eval (Step as) = case eval as of
  Right behavior -> Right $ Behavior [ (f, t, i) | (f,t,a) <- runBehavior behavior | i <- [0..] ]
  Left _         -> Left (Int 0)
