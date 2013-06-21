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
  EMA       :: Double -> F s (P t) Double -> F s (P t) Double
  PrefixSum :: Num a => F s (P t) a -> F s (P t) a
  Sliding   :: Int -> (forall u. F u (P t) a -> F u (P Always) a) -> F s (P t) a -> F s (P t) a
  Delay     :: Int -> F s t a -> F s t a
  Step      :: F s (P t) a -> F s (P t) Int
  Sample    :: Search a -> F s t' a -> F s (P t) a
  (:+)      :: Num a => F s (P t) a -> F s (P t) a -> F s (P t) a
  (:-)      :: Num a => F s (P t) a -> F s (P t) a -> F s (P t) a
  (:*)      :: Num a => F s (P t) a -> F s (P t) a -> F s (P t) a
  (:/)      :: Fractional a => F s (P t) a -> F s (P t) a -> F s (P t) a
  Negate    :: Num a => F s (P t) a -> F s (P t) a
  Abs       :: Num a => F s (P t) a -> F s (P t) a
  Signum    :: Num a => F s (P t) a -> F s (P t) a
  Recip     :: Fractional a => F s (P t) a -> F s (P t) a

  -- * Passes
  Sum :: Num a => F s (P t') a -> F s t a
  Median :: Num a => F s (P t') a -> F s t a
  First :: Num a => F s (P t') a -> F s t a
  Last :: Num a => F s (P t') a -> F s t a
  Min :: Ord a => F s (P t') a -> F s t a
  Max :: Ord a => F s (P t') a -> F s t a
  ArgMin :: Ord a => F s t' a -> F s t' b -> F s t b
  ArgMax :: Ord a => F s t' a -> F s t' b -> F s t b
  -- By       ::                          F t a -> (forall s. F s bs -> F s c) -> F t bs -> F t c
  -- Sorting  ::                       [(F t a, Dir)] -> (forall s. Sorted s => F s bs -> F s c) -> F t bs -> F t c
