{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.CARD.Camo where

import Data.Map (Map)
import qualified Data.Map as Map
import Lens.Micro

import Data.CARD.Effect

data Wrt c = Wrt

class StateOrd c s where
  -- Preorder on states.  This is used in certain situations for
  -- optimizations, but if you don't want to define it, you are safe
  -- leaving it with its default implementation, by which no states
  -- are ordered.
  stateLe :: c -> s -> s -> Bool
  stateLe _ _ _ = False

  -- Check whether a state is a "top" element according to the given
  -- order, meaning that it is greater than or equal to all elements.
  -- Since this is a preorder, there may be multiple top elements.
  stateTop :: c -> s -> Bool
  stateTop _ _ = False

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

class (EffectDom e s, StateOrd c s, EffectOrd c e) => CamoDom c e s

-- Possibly the "downgrade" function should be provided by the Camo
-- implementation, since it involves both the state (a test on a
-- state) and also the effects (a reduced set of blocked effects).
-- Unless it makes sense to state the reduced set of blocked effects
-- as a Camo instead?

instance (StateOrd c s, EffectOrd c e) => EffectOrd c (EConst e s) where
  effectLe c (EConst s1) (EConst s2) = stateLe c s1 s2
  effectLe c _ (EConst s) = stateTop c s
  effectLe c (EModify e1) (EModify e2) = effectLe c e1 e2

instance (EffectDom e s, StateOrd c s, EffectOrd c e)
  => CamoDom c (EConst e s) s

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
  stateLe CLowerBound = (<=)
  stateLe CUpperBound = (>=)

instance (Num n, Ord n) => EffectOrd CCounter (ECounter n) where
  effectLe c e1 e2 =
    additive e1
    && additive e2
    && stateLe c (e1^.addAmt) (e2^.addAmt)

instance (Num n, Ord n) => CamoDom CCounter (ECounter n) n

data CMaybe e s

data CMap k c = CMap k c

instance (Ord k, StateOrd c (Maybe v)) => StateOrd (CMap k c) (Map k v) where
  stateLe (CMap k c) s1 s2 = stateLe c (Map.lookup k s1) (Map.lookup k s2)

instance
  (Ord k, EffectDom e v, EffectOrd c (EMaybe e v))
  => EffectOrd (CMap k c) (EMap k e v) where
  effectLe (CMap k c) e1 e2 = effectLe c (fromKeyE k e1) (fromKeyE k e2)
