{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ParallelListComp #-}
module Data.Time.Series where

import Control.Applicative
import Control.Lens
import Data.Foldable
import Data.Time
import Data.Time.Series.Literal
import Data.Traversable

class Literate t => Sorted t

class Sorted t => Timed t where
  type Delta :: (* -> *) -> *

data Dir = Asc | Desc

data F :: (* -> *) -> * -> * where
  Var      :: Lit a                 => t a -> F t a

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

instance (Literate t, Lit a, Num a) => Num (F t a) where
  (+) = (:+)
  (-) = (:-)
  negate = Negate
  (*) = (:*)
  abs = Abs
  signum = Signum
  fromInteger = lit . fromInteger

instance (Literate t, Lit a, Fractional a) => Fractional (F t a) where
  (/) = (:/)
  recip = Recip
  fromRational = lit . fromRational

_Var :: Lit a => Prism' (F t a) (t a)
_Var = prism' Var $ \a -> case a of
  Var l -> Just l
  _ -> Nothing

instance Literate t => Literate (F t) where
  _Literal = _Var._Literal

replaceMissing :: (Literate t, Lit a) => a -> F t a -> F t a
replaceMissing a p = p `OrElse` lit a

mad :: (Literate t, Fractional a, Lit a, Ord a) => F t a -> F t a
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

