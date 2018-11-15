{-# LANGUAGE DeriveGeneric #-}

module CARD.Lamport
  ( LClock
  , tick
  , untick
  , upto

  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics
import Data.Aeson

import CARD.CvRDT


data LClock i = LClock (Map i Int) deriving (Show,Eq,Ord,Generic)

instance (Ord i) => Semigroup (LClock i) where
  (<>) (LClock m1) (LClock m2) = LClock (Map.unionWith (+) m1 m2)

instance (Ord i) => Monoid (LClock i) where
  mempty = LClock (Map.empty)

instance (ToJSONKey i) => ToJSON (LClock i) where
  toEncoding = genericToEncoding defaultOptions

instance (Ord i, FromJSONKey i) => FromJSON (LClock i)

tick :: (Ord i) => i -> LClock i -> LClock i
tick i (LClock cl) = case Map.lookup i cl of
  Just n -> LClock $ Map.insert i (n + 1) cl
  Nothing -> LClock $ Map.insert i 1 cl

untick :: (Ord i) => i -> LClock i -> LClock i
untick i (LClock cl) = case Map.lookup i cl of
  Just n | n > 0 -> LClock $ Map.insert i (n - 1) cl
         | otherwise -> LClock $ Map.delete i cl
  Nothing -> LClock cl

upto :: (Ord i) => i -> LClock i -> Int
upto i (LClock cl) = case Map.lookup i cl of
  Just n -> n
  Nothing -> 0
