{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.CARD.Maybe
  ( MaybeE
  , JustE (..)
  , insertE
  , adjustE
  , deleteE
  , MaybeC
  , nothingC
  , justC
  , EitherE
  , setLeftE
  , setRightE
  , onLeftE
  , onRightE
  , EitherC
  , leftC
  , rightC
  ) where

import Data.Aeson
import GHC.Generics

import Data.CARD.Classes

type MaybeE e s = ConstE (JustE e) (Maybe s)

newtype JustE e = JustE e deriving (Show,Eq,Ord,Generic)

instance (Semigroup e) => Semigroup (JustE e) where
  JustE e1 <> JustE e2 = JustE (e1 <> e2)

instance (Monoid e) => Monoid (JustE e) where
  mempty = JustE mempty

instance (EffectDom e s) => EffectDom (JustE e) (Maybe s) where
  runEffect (JustE e) (Just s) = Just (runEffect e s)
  runEffect _ s = s

{-| Set the state to @'Data.Maybe.Just' v@. -}
insertE :: v -> MaybeE e v
insertE = ConstE . Just

adjustE :: e -> MaybeE e v
adjustE = ModifyE . JustE

deleteE :: MaybeE e v
deleteE = ConstE Nothing

type EitherE e1 e2 s1 s2 = ConstE (LeftRightE e1 e2) (Either s1 s2)

data LeftRightE e1 e2
  = LeftRightE e1 e2
  deriving (Show,Eq,Ord,Generic)

instance (Semigroup e1, Semigroup e2) => Semigroup (LeftRightE e1 e2) where
  LeftRightE e1 e2 <> LeftRightE f1 f2 = LeftRightE (e1 <> f1) (e2 <> f2)

instance (Monoid e1, Monoid e2) => Monoid (LeftRightE e1 e2) where
  mempty = LeftRightE mempty mempty

instance
  ( EffectDom e1 s1
  , EffectDom e2 s2 )
  => EffectDom (LeftRightE e1 e2) (Either s1 s2) where
  runEffect (LeftRightE e _) (Left s) = Left (runEffect e s)
  runEffect (LeftRightE _ e) (Right s) = Right (runEffect e s)

setLeftE :: s1 -> EitherE e1 e2 s1 s2
setLeftE = ConstE . Left

setRightE :: s2 -> EitherE e1 e2 s1 s2
setRightE = ConstE . Right

onLeftE :: (Monoid e2) => e1 -> EitherE e1 e2 s1 s2
onLeftE e = ModifyE (LeftRightE e idE)

onRightE :: (Monoid e1) => e2 -> EitherE e1 e2 s1 s2
onRightE e = ModifyE (LeftRightE idE e)

data MaybeC c
  = MaybeC (Maybe ()) (Maybe c)
  deriving (Show,Eq,Ord,Generic)

instance (Semigroup c) => Semigroup (MaybeC c) where
  MaybeC n1 m1 <> MaybeC n2 m2 = MaybeC (n1 <> n2) (m1 <> m2)

instance (Monoid c) => Monoid (MaybeC c) where
  mempty = MaybeC mempty mempty

instance (Absorbing c) => Absorbing (MaybeC c) where
  absorb = MaybeC absorb absorb

instance (StateOrd c s) => StateOrd (MaybeC c) (Maybe s) where
  stateLe (MaybeC (Just _) _) Nothing (Just _) = False
  stateLe (MaybeC _ (Just _)) (Just _) Nothing = False
  stateLe (MaybeC _ (Just c)) (Just s1) (Just s2) =
    stateLe c s1 s2
  stateLe _ _ _ = True

instance (EffectOrd c e) => EffectOrd (MaybeC c) (JustE e) where
  effectLe (MaybeC _ (Just c)) (JustE e1) (JustE e2) = effectLe c e1 e2
  effectLe _ _ _ = True

instance (Camo c e s) => Camo (MaybeC c) (JustE e) (Maybe s)

nothingC :: MaybeC c
nothingC = MaybeC (Just ()) Nothing

justC :: c -> MaybeC c
justC c = MaybeC Nothing (Just c)

data EitherC c1 c2 = EitherC (Maybe c1) (Maybe c2)

instance (Semigroup c1, Semigroup c2) => Semigroup (EitherC c1 c2) where
  EitherC c1 c2 <> EitherC d1 d2 = EitherC (c1 <> d1) (c2 <> d2)

instance (Monoid c1, Monoid c2) => Monoid (EitherC c1 c2) where
  mempty = EitherC mempty mempty

instance (Absorbing c1, Absorbing c2) => Absorbing (EitherC c1 c2) where
  absorb = EitherC absorb absorb

instance (StateOrd c1 s1, StateOrd c2 s2) => StateOrd (EitherC c1 c2) (Either s1 s2) where
  stateLe (EitherC (Just _) _) (Left _) (Right _) = False
  stateLe (EitherC _ (Just _)) (Right _) (Left _) = False
  stateLe (EitherC (Just c) _) (Left s1) (Left s2) =
    stateLe c s1 s2
  stateLe (EitherC _ (Just c)) (Right s1) (Right s2) =
    stateLe c s1 s2
  stateLe _ _ _ = True

instance (EffectOrd c1 e1, EffectOrd c2 e2) => EffectOrd (EitherC c1 c2) (LeftRightE e1 e2) where
  effectLe (EitherC m1 m2) (LeftRightE e1 e2) (LeftRightE f1 f2) =
    let a = case m1 of
              Just c1 -> effectLe c1 e1 f1
              Nothing -> True
        b = case m2 of
              Just c2 -> effectLe c2 e2 f2
              Nothing -> True
    in a && b

instance (Camo c1 e1 s1, Camo c2 e2 s2) => Camo (EitherC c1 c2) (LeftRightE e1 e2) (Either s1 s2)

leftC :: c1 -> EitherC c1 c2
leftC c = EitherC (Just c) Nothing

rightC :: c2 -> EitherC c1 c2
rightC c = EitherC Nothing (Just c)
