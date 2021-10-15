{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.CARD.Camo where

import Data.CARD.Effect

data Wrt c = Wrt

class StateOrd c s where
  -- Preorder on states.  This is only used in a few situations for
  -- optimizations, so if you don't want to define it, you are safe
  -- leaving it with its default implementation, by which no states
  -- are ordered.
  stateLe :: c -> s -> s -> Bool
  stateLe _ _ _ = False

class EffectOrd c e where
  -- Preorder on effects.
  effectLe :: c -> e -> e -> Bool

  -- "camoslice _ e1 e2" tries to extract an effect equivalent to e1
  -- from e2.  If successful, it returns Just (e1',e2'), where e1' is
  -- the extracted effect (not necessarily identical to e1) and e2' is
  -- the remainder.
  effectSlice :: Wrt c -> e -> e -> Maybe (e,e)
  effectSlice _ _ _ = Nothing

  -- "camoMerge _ e1 e2" tries to merge the effects.  If successful,
  -- it returns Just e3, where e3 is the merged effect.
  effectMerge :: Wrt c -> e -> e -> Maybe e
  effectMerge _ _ _ = Nothing

class (StateOrd c s, EffectOrd c e) => CamoDom c e s

-- Possibly the "downgrade" function should be provided by the Camo
-- implementation, since it involves both the state (a test on a
-- state) and also the effects (a reduced set of blocked effects).
-- Unless it makes sense to state the reduced set of blocked effects
-- as a Camo instead?

-- The identity relation (or "EQV"), providing an exact snapshot.
type CIdentity = ()

instance (Eq s) => StateOrd CIdentity s where
  stateLe _ s1 s2 = s1 == s2

instance (Eq e, Monoid e) => EffectOrd CIdentity e where
  effectLe _ e1 e2 = e1 == e2
  effectSlice _ e1 e2 | e1 == e2 = Just (e2,mempty)
                      | e1 == mempty = Just (mempty,e2)
                      | otherwise = Nothing
  effectMerge _ e1 e2 | e1 == mempty && e2 == mempty = Just mempty
                      | otherwise = Nothing

-- The universal relation, providing no information, i.e. inconsistent
-- snapshots.
data CUniversal c
  = CUniversal
  | CRelate c

instance (StateOrd c e) => StateOrd (CUniversal c) e where
  stateLe CUniversal = \_ _ -> True
  stateLe (CRelate c) = stateLe c

cuwrt :: Wrt (CUniversal c) -> Wrt c
cuwrt Wrt = Wrt

instance (EffectOrd c e) => EffectOrd (CUniversal c) e where
  effectLe CUniversal = \_ _ -> True
  effectLe (CRelate c) = effectLe c
  effectSlice wrt e1 e2 = effectSlice (cuwrt wrt) e1 e2
  effectMerge wrt e1 e2 = effectMerge (cuwrt wrt) e1 e2

instance (CamoDom c e s) => CamoDom (CUniversal c) e s

data CCounter 
  = CLowerBound
  | CUpperBound
  | CNotNegative
  | CNotPositive

instance (Num n, Ord n) => StateOrd CCounter n where
  stateLe = \case
    CLowerBound -> (<=)
    CUpperBound -> (>=)
    CNotNegative -> \x s -> (s >= 0) || not (x >= 0)
    CNotPositive -> \x s -> (s <= 0) || not (x <= 0)
