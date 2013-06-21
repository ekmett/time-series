{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Time.Series.Periodicity
  ( Periodicity(..)
  , Timing(..)
--   , Periodic(..)
#if __GLASGOW_HASKELL__ >= 706
  , reifyPeriodicity
#endif
  ) where

import Data.Data
import Data.Ix
#if __GLASGOW_HASKELL__ >= 706
import Data.Proxy
import GHC.TypeLits
#endif

data Periodicity = Always | Daily | Weekly | Monthly | Yearly
  deriving (Eq,Ord,Show,Read,Data,Enum,Bounded,Ix,Typeable)

data Timing = V | P Periodicity
  deriving (Eq,Ord,Show,Read,Data,Typeable)

#if __GLASGOW_HASKELL__ >= 706
newtype instance Sing (m :: Periodicity) = SPeriodicity Periodicity

instance SingE (Kind :: Periodicity) Periodicity where
  fromSing (SPeriodicity m) = m

instance SingI Always   where sing = SPeriodicity Always
instance SingI Daily   where sing = SPeriodicity Daily
instance SingI Weekly  where sing = SPeriodicity Weekly
instance SingI Monthly where sing = SPeriodicity Monthly
instance SingI Yearly  where sing = SPeriodicity Yearly

reifyPeriodicity :: Periodicity -> (forall (s :: Periodicity). SingI s => Proxy s -> r) -> r
reifyPeriodicity Always f  = f (Proxy :: Proxy Always)
reifyPeriodicity Daily f   = f (Proxy :: Proxy Daily)
reifyPeriodicity Weekly f  = f (Proxy :: Proxy Weekly)
reifyPeriodicity Monthly f = f (Proxy :: Proxy Monthly)
reifyPeriodicity Yearly f  = f (Proxy :: Proxy Yearly)
{-# INLINE reifyPeriodicity #-}
#endif
