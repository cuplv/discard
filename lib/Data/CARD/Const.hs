{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.CARD.Const where

import Data.CARD.Classes
import Data.CARD.InfSet (InfSet)
import qualified Data.CARD.InfSet as IS

import Data.Aeson
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
  = ConstC { setVals :: InfSet s, lowerC :: c }
  deriving (Show,Eq,Ord,Generic)

instance (ToJSON c, ToJSON s) => ToJSON (ConstC c s)
instance (Ord s, ToJSON c, ToJSONKey c, ToJSON s, ToJSONKey s) => ToJSONKey (ConstC c s)
instance (Ord s, FromJSON c, FromJSON s) => FromJSON (ConstC c s)
instance (Ord s, FromJSON c, FromJSONKey c, FromJSON s, ToJSONKey s) => FromJSONKey (ConstC c s)

instance (Ord s, Semigroup c) => Semigroup (ConstC c s) where
  ConstC s1 c1 <> ConstC s2 c2 =
    ConstC (IS.union s1 s2) (c1 <> c2)

instance (Ord s, Monoid c) => Monoid (ConstC c s) where
  mempty = ConstC IS.empty mempty

instance (Ord s, Meet c) => Meet (ConstC c s) where
  meet (ConstC s1 c1) (ConstC s2 c2) =
    ConstC (s1 `IS.intersection` s2) (c1 `meet` c2)

  ConstC s1 c1 <=? ConstC s2 c2 = (s1 `IS.isSubsetOf` s2) && (c1 <=? c2)

instance (Ord s, BMeet c) => BMeet (ConstC c s) where
  meetId = ConstC IS.universal meetId

instance (Ord s, Split c) => Split (ConstC c s) where
  split (ConstC s1 c1) (ConstC s2 c2) =
    if s2 `IS.isSubsetOf` s1
       then ConstC s1 <$> split c1 c2
       else Nothing

instance (Ord s, Meet c, Cap c e, EffectDom e s, Split c) => Cap (ConstC c s) (ConstE e s) where
  mincap (ConstE s) = ConstC (IS.singleton s) mempty
  mincap (ModifyE e) = ConstC IS.empty (mincap e)

constC :: (Ord s, Monoid c) => [s] -> ConstC c s
constC ss = ConstC (IS.fromList ss) mempty

constC' :: (Monoid c) => InfSet s -> ConstC c s
constC' s = ConstC s mempty

modifyC :: c -> ConstC c s
modifyC c = ConstC IS.empty c
