{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Data.Time.Series.Model where

import Data.Time
import Data.Time.Series.Periodicity
import Data.Vector

data Model :: Timing -> * -> * where
  Given    :: a -> Model t a
  Periodic :: Day -> Int -> Vector a -> Model (P t) a -- * Starting day, difference in P units between points 
  Variant  :: [(Day, a)] -> Model V a                 -- * List of impulses 

instance Functor (Model t) where
  fmap f (Given a) = Given (f a)
  fmap f (Periodic d i v) = Periodic d i (fmap f v)
  fmap f (Variant xs) = Variant [ (i, f a) | (i, a) <- xs ]
