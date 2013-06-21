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
  LookBack :: Int -> Periodicity -> Search a -> Search a
  LookForward :: Int -> Periodicity -> Search a -> Search a
  LookNear :: Int -> Periodicity -> Search a -> Search a
  OrElse :: a -> Search a

data F :: * -> Timing -> * -> * where
  Var       :: Lit a => Model t a -> F s t a
  EMA       :: Double -> F s (P u) Double -> F s (P u) Double
  PrefixSum :: Num a => F s (P u) a -> F s (P u) a
  Sliding   :: Int -> (forall s'. F s' (P u) a -> F s' (P Always) a) -> F s (P u) a -> F s (P u) a
  Delay     :: Int -> F s t a -> F s t a
  Step      :: F s (P u) a -> F s (P u) Int
  Sample    :: Search a -> F s t' a -> F s (P u) a
  (:+)      :: Num a => F s (P u) a -> F s (P u) a -> F s (P u) a
  (:-)      :: Num a => F s (P u) a -> F s (P u) a -> F s (P u) a
  (:*)      :: Num a => F s (P u) a -> F s (P u) a -> F s (P u) a
  (:/)      :: Fractional a => F s (P u) a -> F s (P u) a -> F s (P u) a
  Negate    :: Num a => F s (P u) a -> F s (P u) a
  Abs       :: Num a => F s (P u) a -> F s (P u) a
  Signum    :: Num a => F s (P u) a -> F s (P u) a
  Recip     :: Fractional a => F s (P u) a -> F s (P u) a
  Filter    :: F s (P t) Bool -> F s (P t) a -> F s V a

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
  First     :: F s (P u) a -> F s t a
  Last      :: F s (P u) a -> F s t a
  Min       :: Ord a => F s (P u) a -> F s t a
  Max       :: Ord a => F s (P u) a -> F s t a
  ArgMin    :: Ord a => F s t' a -> F s t' b -> F s t b
  ArgMax    :: Ord a => F s t' a -> F s t' b -> F s t b
  -- By       ::                          F t a -> (forall s. F s bs -> F s c) -> F t bs -> F t c
  -- Sorting  ::                       [(F t a, Dir)] -> (forall s. Sorted s => F s bs -> F s c) -> F t bs -> F t c
