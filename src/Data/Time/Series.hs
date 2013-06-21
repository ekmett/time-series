{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ParallelListComp #-}
module Data.Time.Series where

-- import Control.Lens
-- import Data.Time
import Data.Time.Series.Literal
import Data.Time.Series.Model
import Data.Time.Series.Periodicity

data Search a where
  Lookback :: Int -> Periodicity -> Search a -> Search a
  Lookforward :: Int -> Periodicity -> Search a -> Search a
  Looknear :: Int -> Periodicity -> Search a -> Search a
  OrElse :: a -> Search a

data F :: * -> Timing -> * -> * where
  Var       :: Lit a => Model t a -> F s t a
  EMA       :: Periodic t => Double -> F s t Double -> F s t Double
  PrefixSum :: (Periodic t, Num a) => F s t a -> F s t a
  Sliding   :: Periodic t => Int -> (forall u. F u t a -> F u G a) -> F s t a -> F s t a
  Delay     :: Int -> F s t a -> F s t a
  Step      :: Periodic t => F s t a -> F s t Int
  Sample    :: Search a -> F s t' a -> F s (P t) a
  (:+)      :: (Periodic t, Num a) => F s t a -> F s t a -> F s t a
  (:-)      :: (Periodic t, Num a) => F s t a -> F s t a -> F s t a
  (:*)      :: (Periodic t, Num a) => F s t a -> F s t a -> F s t a
  (:/)      :: (Periodic t, Fractional a) => F s t a -> F s t a -> F s t a
  Negate    :: (Periodic t, Num a) => F s t a -> F s t a
  Abs       :: (Periodic t, Num a) => F s t a -> F s t a
  Signum    :: (Periodic t, Num a) => F s t a -> F s t a
  Recip     :: (Periodic t, Fractional a) => F s t a -> F s t a

  -- * Passes
  Sum :: (Periodic t', Num a) => F s t' a -> F s t a
  Median :: (Periodic t', Num a) => F s t' a -> F s t a
  First :: Num a => F s t' a -> F s t a -- need an ordered constraint, so can't be used in a By 
  Last :: Num a => F s t' a -> F s t a
  Min :: F s t' a -> F s t a
  Max :: F s t' a -> F s t a
  ArgMin :: (Periodic t', Num a) => F s t' a -> F s t' b -> F s t b
  ArgMax :: (Periodic t', Num a) => F s t' a -> F s t' b -> F s t b
  -- By       ::                          F t a -> (forall s. F s bs -> F s c) -> F t bs -> F t c
  -- Sorting  ::                       [(F t a, Dir)] -> (forall s. Sorted s => F s bs -> F s c) -> F t bs -> F t c

{-
_Var :: Lit a => Prism' (F t a) (Model t a)
_Var = prism' Var $ \a -> case a of
  Var l -> Just l
  _ -> Nothing

instance Literate (F t) where
  _Literal = _Var._Literal
-}

-- foo = Var (Periodic startingDate 1 (Vector.replicate 100 4)) :: F Model (P Monthly) a

{-
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
-}
