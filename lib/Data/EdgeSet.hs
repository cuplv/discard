{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.EdgeSet where

import Data.Set (Set)
import qualified Data.Set as Set

data EdgeSet e = EdgeSet (Set (Effect e)) deriving (Eq, Ord)

data Effect e = Effect e (EdgeSet e) deriving (Eq, Ord)

instance (Show e) => Show (Effect e) where
  show (Effect e es) = show e ++ " < " ++ show es

instance (Show e) => Show (EdgeSet e) where
  show (EdgeSet es) = "{ " ++ concat (map ((++ ", ") . show) (Set.toList es)) ++ " }"

emptyEdgeSet :: (Ord e) => EdgeSet e
emptyEdgeSet = EdgeSet mempty

emit :: (Ord e) => Effect e -> EdgeSet e -> EdgeSet e
emit ef@(Effect e es1) (EdgeSet s2) = 
  EdgeSet (ef `Set.insert` Set.filter (not . seen es1) s2)

syncAdd :: (Ord e) => e -> EdgeSet e -> EdgeSet e
syncAdd e es = EdgeSet (Set.singleton $ Effect e es)

bareEffect :: (Ord e) => e -> Effect e
bareEffect e = Effect e emptyEdgeSet

seen :: (Ord e) => EdgeSet e -> Effect e -> Bool
seen (EdgeSet s) e = 
  if Set.member e s
     then True
     else or . Set.toList . Set.map (\(Effect _ es) -> seen es e) $ s

eff :: (Ord e) => e -> Effect e
eff = bareEffect

class ToEdgeSet a where
  type EffectType a
  toEdgeSet :: a -> EdgeSet (EffectType a)

instance ToEdgeSet (EdgeSet e) where
  type EffectType (EdgeSet e) = e
  toEdgeSet = id
  
instance ToEdgeSet (Effect e) where
  type EffectType (Effect e) = e
  toEdgeSet = EdgeSet . Set.singleton

instance ToEdgeSet (Set (Effect e)) where
  type EffectType (Set (Effect e)) = e
  toEdgeSet = EdgeSet
  
instance (Ord e) => ToEdgeSet [Effect e] where
  type EffectType [Effect e] = e
  toEdgeSet = EdgeSet . Set.fromList

(<:) :: (Ord e, ToEdgeSet a, EffectType a ~ e) => e -> a -> Effect e
(<:) e es = Effect e (toEdgeSet es)

(<#) :: (Ord e) => e -> e -> Effect e
(<#) e1 e2 = Effect e1 (toEdgeSet (Effect e2 emptyEdgeSet))

tes :: (ToEdgeSet a) => a -> EdgeSet (EffectType a)
tes = toEdgeSet

infixr <:
infixr <#
