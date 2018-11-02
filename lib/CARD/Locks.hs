{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CARD.Locks
  ( Locks
  -- * Modify the locks
  , request
  , grant
  , release
  -- * Examine the locks
  , permitted
  , requested
  , holding
  , confirmed
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State
import GHC.Generics hiding (Rep)
import Data.Aeson
import Data.List (nub)
import Data.Foldable (foldl')

import Control.Monad.Free

import CARD.CvRDT
import CARD.Store

data Locks i s = Locks 
  { locks :: Map i (Int, Conref s, Set i) } 
  deriving (Eq,Ord,Generic)

instance (ToJSONKey i, ToJSON i, ToJSON (Cr s)) => ToJSON (Locks i s)
instance (Ord i, FromJSONKey i, FromJSON i, Ord (Cr s), FromJSON (Cr s)) => FromJSON (Locks i s)

instance (Ord i, Store s) => Semigroup (Locks i s) where
  (<>) (Locks m1) (Locks m2) = 
    Locks $ foldl' (\m k -> case (Map.lookup k m1, Map.lookup k m2) of
      (Just (i1,c1,s1),Just (i2,c2,s2)) -> case compare i1 i2 of
        EQ -> if c1 == c2
                 then Map.insert k (i1, c1, s1 `Set.union` s2) m
                 else Map.insert k (i1, c1 |&| c2, Set.empty) m
        LT -> Map.insert k (i2,c2,s2) m
        GT -> Map.insert k (i1,c1,s1) m
      (Just v1,Nothing) -> Map.insert k v1 m
      (Nothing,Just v2) -> Map.insert k v2 m) 
                   mempty (nub $ Map.keys m1 ++ Map.keys m2)

instance (Ord i, Store s) => Monoid (Locks i s) where
  mempty = Locks (mempty)

instance (Ord i, Store s, Applicative m) => CvRDT () (Locks i s) m where
  merge _ s1 s2 = pure (s1 <> s2)
  cvempty _ = pure mempty

-- | Get the current request index for an identity
reqIndex :: (Ord i, Store s) => i -> Locks i s -> Int
reqIndex i (Locks m) = case Map.lookup i m of
                         Just (n,_,_) -> n
                         Nothing -> 0

-- | Request a new/stronger lock
request :: (Ord i, Store s) => i -> Conref s -> Locks i s -> Locks i s
request i c m = m <> (Locks $ Map.singleton i (reqIndex i m, c, mempty))

-- | Grant a request
grant :: (Ord i, Store s) 
      => i -- ^ Granting identity
      -> i -- ^ Identity whose request is being granted
      -> Locks i s 
      -> Locks i s
grant ig ir (Locks m) = 
  Locks $ Map.adjust (\(n,c,s) -> if c /= crT
                                     then (n,c,Set.insert ig s)
                                     else (n,c,s)) ir m

-- | Release a lock
release :: (Ord i, Store s) => i -> Locks i s -> Locks i s
release i m = m <> (Locks $ Map.singleton i (reqIndex i m + 1, crT, mempty))

-- | Check if effect is blocked by current locks
permitted :: (Ord i, Store s) => i -> Effect s -> Locks i s -> Bool
permitted i e =
  not
  . or 
  . map (\(n,c,s) -> checkBlock c e && Set.member i s)
  . Map.elems
  . locks

-- | Check if lock has been requested
requested :: (Ord i, Store s) => i -> Conref s -> Locks i s -> Bool
requested i c1 (Locks m) = case Map.lookup i m of
  Just (_,c2,_) -> c2 `impl` c1
  Nothing -> if c1 == crT
                then True
                else False

holding :: (Ord i, Store s) => i -> Locks i s -> Bool
holding i (Locks m) = case Map.lookup i m of
  Just (_,c,_) -> if c == crT
                     then False
                     else True
  Nothing -> True

-- | Check if currently requested lock has been confirmed by all
-- participating identities
confirmed :: (Ord i, Store s) 
          => i -- ^ Identity to check the requested lock of
          -> [i] -- ^ List of other participating identities
          -> Locks i s 
          -> Bool
confirmed self others (Locks m) = 
  case Map.lookup self m of
    Just (_,c,s) -> 
      c == crT 
      || Set.fromList others `Set.isSubsetOf` s
    Nothing -> True
