{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.LamportClock
  ( LClock
  , tick
  , untick
  , upto

  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics
import Data.Aeson

import Data.CvRDT


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


data LHist i r d = LHist (LClock i) [(i,d)] (Maybe (Ref r))

deriving instance (Eq i, Eq (Ref r), Eq d) => Eq (LHist i r d)
deriving instance (Ord i, Ord (Ref r), Ord d) => Ord (LHist i r d)

class (Eq (Ref r)) => LHistStore r where
  data Ref r
  chunkSize :: r -> Int

class (Ord i, Ord d, LHistStore r, Monad m) => LHistM i r d m where
  loadChunk :: r -> Ref r -> m [(i,d)]
  arb :: r -> (i,d) -> (i,d) -> m Bool

putInOrder :: (LHistM i r d m) => r -> (i,d) -> (i,d) -> m [(i,d)]
putInOrder r e1 e2 = arb r e1 e2 >>= \case
  True -> return [e2,e1]
  False -> return [e1,e2]

instance (Ord (LHist i r d), LHistM i r d m) => CvRDT r (LHist i r d) m where
  cvmerge r (LHist c1 ((i1,d1):e1) t1) (LHist c2 ((i2,d2):e2) t2)

    | c1 == c2 = return (LHist c1 ((i1,d1):e1) t1)

    | upto i1 c2 >= upto i1 c1 = cvmerge r (LHist (untick i1 c1) e1 t1) (LHist c2 ((i2,d2):e2) t2)
    | upto i2 c1 >= upto i2 c2 = cvmerge r (LHist c1 ((i1,d1):e1) t1)   (LHist (untick i2 c2) e2 t2)
    | otherwise = do (LHist c' e' t') <- cvmerge r (LHist (untick i1 c1) e1 t1) (LHist (untick i2 c2) e2 t2)
                     es <- putInOrder r (i1,d1) (i2,d2)
                     return (LHist (tick i1 . tick i2 $ c') (es ++ e') t')
