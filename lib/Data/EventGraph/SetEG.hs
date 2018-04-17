{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.EventGraph.SetEG where

import Data.Set (Set)
import qualified Data.Set as Set

data SetEG e = SetEG (Set (SetEff e)) deriving (Eq, Ord)

data SetEff e = SetEff e (SetEG e) deriving (Eq, Ord)

instance (Show e) => Show (SetEff e) where
  show (SetEff e es) = show e ++ " < " ++ show es

instance (Show e) => Show (SetEG e) where
  show (SetEG es) = "{ " ++ concat (map ((++ ", ") . show) (Set.toList es)) ++ " }"

emptySetEG :: (Ord e) => SetEG e
emptySetEG = SetEG mempty

emit :: (Ord e) => SetEff e -> SetEG e -> SetEG e
emit ef@(SetEff e es1) (SetEG s2) = 
  SetEG (ef `Set.insert` Set.filter (not . seen es1) s2)

syncAdd :: (Ord e) => e -> SetEG e -> SetEG e
syncAdd e es = SetEG (Set.singleton $ SetEff e es)

bareSetEff :: (Ord e) => e -> SetEff e
bareSetEff e = SetEff e emptySetEG

seen :: (Ord e) => SetEG e -> SetEff e -> Bool
seen (SetEG s) e = 
  if Set.member e s
     then True
     else or . Set.toList . Set.map (\(SetEff _ es) -> seen es e) $ s

eff :: (Ord e) => e -> SetEff e
eff = bareSetEff

class ToSetEG a where
  type EffectType a
  toSetEG :: a -> SetEG (EffectType a)

instance ToSetEG (SetEG e) where
  type EffectType (SetEG e) = e
  toSetEG = id
  
instance ToSetEG (SetEff e) where
  type EffectType (SetEff e) = e
  toSetEG = SetEG . Set.singleton

instance ToSetEG (Set (SetEff e)) where
  type EffectType (Set (SetEff e)) = e
  toSetEG = SetEG
  
instance (Ord e) => ToSetEG [SetEff e] where
  type EffectType [SetEff e] = e
  toSetEG = SetEG . Set.fromList

(<:) :: (Ord e, ToSetEG a, EffectType a ~ e) => e -> a -> SetEff e
(<:) e es = SetEff e (toSetEG es)

(<#) :: (Ord e) => e -> e -> SetEff e
(<#) e1 e2 = SetEff e1 (toSetEG (SetEff e2 emptySetEG))

tes :: (ToSetEG a) => a -> SetEG (EffectType a)
tes = toSetEG

infixr <:
infixr <#

