{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module CARD.EventGraph.SetEG where

import Control.Monad.Identity
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe

import CARD.EventGraph

data SetEG e = SetEG (Set (SetEff e)) deriving (Eq, Ord)

data SetEff e = SetEff e (SetEG e) deriving (Eq, Ord)

instance (Show e) => Show (SetEff e) where
  show (SetEff e es) = show e ++ " < " ++ show es

instance (Show e) => Show (SetEG e) where
  show (SetEG es) = "{ " ++ concat (map ((++ ", ") . show) (Set.toList es)) ++ " }"

------------------------------------------------------------------------

emptySetEG :: (Ord e) => SetEG e
emptySetEG = SetEG mempty

instance (Ord e) => EventGraph SetEG e where
  empty = emptySetEG

-- Any monad can host a SetEG, and any event with Ord can be stored
instance (Monad m, Ord e) => MonadEG SetEG e m where
  append e = return . syncAdd e
  merge g1 g2 = return $ mergeSetEG g1 g2
  edge (SetEG es) = 
    return 
    . map (\(SetEff e g) -> (e,g)) 
    . Set.toList 
    $ es
  pop (SetEG es) = do let (mes,es') = Set.splitAt 1 es
                          me = listToMaybe (Set.toList mes)
                      case me of
                        Just (SetEff e g) -> do g' <- merge g (SetEG es')
                                                return (Just (e,g'))
                        Nothing -> return Nothing

emit :: (Ord e) => SetEff e -> SetEG e -> SetEG e
emit ef@(SetEff e es1) (SetEG s2) = 
  SetEG (ef `Set.insert` Set.filter (not . seen es1) s2)

mergeSetEG :: (Ord e) => SetEG e -> SetEG e -> SetEG e
mergeSetEG eg1@(SetEG s1) eg2@(SetEG s2) = SetEG $
  Set.union 
    (Set.filter (not.seen' eg2) s1)
    (Set.filter (not.seen' eg1) s2)

syncAdd :: (Ord e) => e -> SetEG e -> SetEG e
syncAdd e es = SetEG (Set.singleton $ SetEff e es)

bareSetEff :: (Ord e) => e -> SetEff e
bareSetEff e = SetEff e emptySetEG

seen' :: (Ord e) => SetEG e -> SetEff e -> Bool
seen' (SetEG s) e =
  or . Set.toList . Set.map (\(SetEff _ es) -> seen es e) $ s

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
