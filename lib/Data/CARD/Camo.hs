{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.CARD.Camo where

import Data.CARD.Effect

data Wrt c = Wrt

class CamoDom c e where
  camoLe :: c -> e -> e -> Bool
  
  -- "camoslice _ e1 e2" tries to extract an effect equivalent to e1
  -- from e2.  If successful, it returns Just (e1',e2'), where e1' is
  -- the extracted effect (not necessarily identical to e1) and e2' is
  -- the remainder.
  camoSlice :: Wrt c -> e -> e -> Maybe (e,e)

  -- "camoMerge _ e1 e2" tries to merge the effects.  If successful,
  -- it returns Just e3, where e3 is the merged effect.
  camoMerge :: Wrt c -> e -> e -> Maybe e

-- The identity relation (or "EQV"), providing an exact snapshot.
type CIdentity = ()

instance (Eq e, Monoid e) => CamoDom () e where
  camoLe () e1 e2 = e1 == e2
  camoSlice _ e1 e2 | e1 == e2 = Just (e2,mempty)
                    | e1 == mempty = Just (mempty,e2)
                    | otherwise = Nothing
  camoMerge _ e1 e2 | e1 == mempty && e2 == mempty = Just mempty
                    | otherwise = Nothing

-- The universal relation, providing no information, i.e. inconsistent
-- snapshots.
data CUniversal c
  = CUniversal
  | CRelate c

cUniversalWrt :: Wrt (CUniversal c) -> Wrt c
cUniversalWrt Wrt = Wrt

instance (CamoDom c e) => CamoDom (CUniversal c) e where
  camoLe CUniversal _ _ = True
  camoSlice wrt e1 e2 = camoSlice (cUniversalWrt wrt) e1 e2
  camoMerge wrt e1 e2 = camoMerge (cUniversalWrt wrt) e1 e2

data CCounter 
  = CUpperBound
  | CLowerBound
  | CNotNegative
  | CNotPositive
