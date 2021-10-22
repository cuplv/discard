{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.CARD.Const where

import Data.CARD.Classes

import Data.Aeson
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics

{-| t'ConstE' wraps an effect domain (@e@), adding an effect that
    replaces the current state with a given value.

@
'eFun' ('ConstE' s) = 'Data.Function.const' s = (\\_ -> s)
@

    Effects from the wrapped domain can be used with 'ModifyE'.

@
'eFun' ('ModifyE' e) s = 'eFun' e s
@
-} 
data ConstE e s
  = ConstE s
  | ModifyE e
  deriving (Show,Eq,Ord,Generic)

instance (ToJSON e, ToJSON s) => ToJSON (ConstE e s) where
  toEncoding = genericToEncoding defaultOptions
instance (FromJSON e, FromJSON s) => FromJSON (ConstE e s)

instance (EffectDom e s) => Semigroup (ConstE e s) where
  ConstE s <> _ = ConstE s
  ModifyE e2 <> ModifyE e1 = ModifyE (e2 <> e1)
  ModifyE e <> ConstE s = ConstE (eFun e s)

instance (EffectDom e s) => Monoid (ConstE e s) where
  mempty = ModifyE mempty

instance (Eq s, EffectSlice e, EffectDom e s) => EffectSlice (ConstE e s) where
  effectSlice (ConstE s1) (ConstE s2) | s1 == s2 = Just (ConstE s1,idE)
  effectSlice (ModifyE e1) (ModifyE e2) = case effectSlice e1 e2 of
    Just (e1',e2') -> Just (ModifyE e1', ModifyE e2')
    Nothing -> Nothing
  effectSlice _ _ = Nothing
  effectMerge (ConstE s1) (ConstE s2) | s1 == s2 = Just (ConstE s1)
  effectMerge (ModifyE e1) (ModifyE e2) = ModifyE <$> effectMerge e1 e2
  effectMerge _ _ = Nothing

instance (EffectDom e s) => EffectDom (ConstE e s) s where
  eFun (ConstE s) = const s
  eFun (ModifyE e) = eFun e

data ConstC c s
  = ConstC { matchVals :: Set s, escapeVals :: Set s, lowerC :: c }
  deriving (Show,Eq,Ord,Generic)

instance (ToJSON c, ToJSON s) => ToJSON (ConstC c s)
instance (ToJSON c, ToJSONKey c, ToJSON s, ToJSONKey s) => ToJSONKey (ConstC c s)
instance (Ord s, FromJSON c, FromJSON s) => FromJSON (ConstC c s)
instance (Ord s, FromJSON c, FromJSONKey c, FromJSON s, ToJSONKey s) => FromJSONKey (ConstC c s)

instance (Ord s, Semigroup c) => Semigroup (ConstC c s) where
  ConstC m1 s1 c1 <> ConstC m2 s2 c2 =
    ConstC (m1 `Set.intersection` m2) (s1 `Set.intersection` s2) (c1 <> c2)

instance (StateOrd c s) => StateOrd (ConstC c s) s where
  -- stateLe (ConstLower c) = stateLe c

instance (Absorbing c, StateOrd c s, EffectOrd c) => EffectOrd (ConstC c s) where
  type Ef (ConstC c s) = ConstE (Ef c) s
  -- effectLe (AgreeOn ss) ()
  -- effectLe (ConstLower c) (ConstE s1) (ConstE s2) = stateLe c s1 s2
  -- effectLe (ConstLower c) _ (ConstE s) = stateTop c s
  -- effectLe (ConstLower c) (ModifyE e1) (ModifyE e2) = effectLe c e1 e2

  intoCap (ConstE s) = ConstC Set.empty (Set.singleton s) idC
  intoCap (ModifyE e) = ConstC Set.empty Set.empty (intoCap e)
