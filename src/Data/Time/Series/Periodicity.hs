{-# LANGUAGE DataKinds #-}
module Data.Time.Series.Periodicity
  ( Periodicity(..)
  , Timing(..)
  ) where

data Periodicity = Daily | Weekly | Monthly | Yearly

data Timing = V | P Periodicity
