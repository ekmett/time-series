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

import Data.Time.Series.Literal
import Data.Time.Series.Periodicity

data Search a where
  LookBack :: Int -> Periodicity -> Search a -> Search a
  LookForward :: Int -> Periodicity -> Search a -> Search a
  LookNear :: Int -> Periodicity -> Search a -> Search a
  OrElse :: a -> Search a


data F :: (Timing -> * -> *) -> Timing -> * -> * where
  Var       :: Lit a => s t a -> F s t a

  EMA       :: Double -> F s (P (T u)) Double -> F s (P (T u)) Double
  PrefixSum :: Num a => F s (P (T u)) a -> F s (P (T u)) a
  Sliding   :: Int -> (forall s'. F s' (P (T u)) a -> F s' (P (T Always)) a) -> F s (P (T u)) a -> F s (P (T u)) a
  Delay     :: Int -> F s t a -> F s t a -- problematic, this should work on P (T u) or V, but not (P B)!
  Step      :: F s (P (T u)) a -> F s (P (T u)) Int
  Sample    :: Search a -> F s t' a -> F s (P u) a -- should require t' to not be (P B), parameterize the Search?
  Variant   :: F s (P (T u)) a -> F s V a
  (:+)      :: Num a => F s (P u) a -> F s (P u) a -> F s (P u) a
  (:-)      :: Num a => F s (P u) a -> F s (P u) a -> F s (P u) a
  (:*)      :: Num a => F s (P u) a -> F s (P u) a -> F s (P u) a
  (:/)      :: Fractional a => F s (P u) a -> F s (P u) a -> F s (P u) a
  Negate    :: Num a => F s t a -> F s t a
  Abs       :: Num a => F s t a -> F s t a
  Signum    :: Num a => F s t a -> F s t a
  Recip     :: Fractional a => F s t a -> F s t a
  Filter    :: F s (P u) Bool -> F s (P u) a -> F s V a

  (:==)     :: Eq a => F s (P u) a -> F s (P u) a -> F s (P u) Bool
  (:/=)     :: Eq a => F s (P u) a -> F s (P u) a -> F s (P u) Bool
  (:<=)     :: Ord a => F s (P u) a -> F s (P u) a -> F s (P u) Bool
  (:<)      :: Ord a => F s (P u) a -> F s (P u) a -> F s (P u) Bool
  (:>=)     :: Ord a => F s (P u) a -> F s (P u) a -> F s (P u) Bool
  (:>)      :: Ord a => F s (P u) a -> F s (P u) a -> F s (P u) Bool
  (:&&)     :: F s (P u) Bool -> F s (P u) Bool -> F s (P u) Bool
  (:||)     :: F s (P u) Bool -> F s (P u) Bool -> F s (P u) Bool
  Not       :: F s t Bool -> F s t Bool

  -- * Passes
  Sum       :: Num a => F s (P u) a -> F s t a
  Median    :: Num a => F s (P u) a -> F s t a
  First     :: F s (P (T u)) a -> F s t a
  Last      :: F s (P (T u)) a -> F s t a
  Min       :: Ord a => F s (P u) a -> F s t a
  Max       :: Ord a => F s (P u) a -> F s t a
  ArgMin    :: Ord a => F s (P u) a -> F s (P u) b -> F s t b
  ArgMax    :: Ord a => F s (P u) a -> F s (P u) b -> F s t b
  -- need Count
  By        :: F s (P u) a -> (forall s'. F s' (P B) a -> F s' (P u) b -> F s' (P (T Always)) c)) -> F s (P u) b -> F s (P u) c
  -- Sorting  ::                       [(F t a, Dir)] -> (forall s. Sorted s => F s bs -> F s c) -> F t bs -> F t c

class Periodic t where
  ema :: Double -> F s t Double -> F s t Double

instance Periodic (P (T u)) where
  ema = EMA

{-
mean x = sum x / (last step + 1)

mad f = median (abs (f - median f))
-}
