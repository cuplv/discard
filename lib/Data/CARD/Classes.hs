{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Data.CARD.Classes
  ( EffectDom (..)
  , EffectSlice (..)
  , idE
  , Absorbing (..)
  , StateOrd (..)
  , EffectOrd (..)
  , Camo (..)
  , uniC
  , idC
  , impl
  , UniversalC (..)
  , Split (..)
  , Meet (..)
  , BMeet (..)
  , Cap (..)
  ) where

import Data.Aeson
import GHC.Generics

class (Eq e, Monoid e) => EffectSlice e where
  -- "camoslice _ e1 e2" tries to extract an effect equivalent to e1
  -- from e2.  If successful, it returns Just (e1',e2'), where e1' is
  -- the extracted effect (not necessarily identical to e1) and e2' is
  -- the remainder.
  effectSlice :: e -> e -> Maybe (e,e)
  effectSlice e1 e2 | e1 == idE = Just (e1,e2)
                    | e1 == e2 = Just (e2,idE)
                    | otherwise = Nothing
  -- "camoMerge _ e1 e2" tries to merge the effects.  If successful,
  -- it returns Just e3, where e3 is the merged effect.
  effectMerge :: e -> e -> Maybe e
  effectMerge e1 e2 | e1 == idE = Just e2
                    | e2 == idE = Just e1
                    | otherwise = Nothing

  effectPartition :: Int -> e -> [e]
  effectPartition _ e = [e]

{-| An 'EffectDom' is a domain of effects (@e@) on some state type
    (@s@), in which each effect denotes (by 'eFun') a pure
    function on a state value.  The effects must form a
    'Data.Monoid.Monoid' according to the following laws:

@
\-\- Identity
'eFun' 'Data.Monoid.mempty' = 'Data.Function.id'

\-\- Composition
'eFun' (e2 'Data.Semigroup.<>' e1) = 'eFun' e2 'Data.Function..' 'eFun' e1
@

    Note that 'Data.Semigroup.<>' composes effects right-to-left, just
    like function composition.
-}
class (Monoid e) => EffectDom e s where
  eFun :: e -> s -> s

instance EffectDom () s where
  eFun () = id

instance
  ( EffectDom e1 s1
  , EffectDom e2 s2 )
  => EffectDom (e1,e2) (s1,s2) where
  eFun (e1,e2) (s1,s2) =
    ( eFun e1 s1
    , eFun e2 s2 )

instance
  ( EffectDom e1 s1
  , EffectDom e2 s2
  , EffectDom e3 s3 )
  => EffectDom (e1,e2,e3) (s1,s2,s3) where
  eFun (e1,e2,e3) (s1,s2,s3) =
    ( eFun e1 s1
    , eFun e2 s2
    , eFun e3 s3 )

instance
  ( EffectDom e1 s1
  , EffectDom e2 s2
  , EffectDom e3 s3
  , EffectDom e4 s4 )
  => EffectDom (e1,e2,e3,e4) (s1,s2,s3,s4) where
  eFun (e1,e2,e3,e4) (s1,s2,s3,s4) =
    ( eFun e1 s1
    , eFun e2 s2
    , eFun e3 s3
    , eFun e4 s4 )

{-| The identity effect, a synonym for 'Data.Monoid.mempty'. -}
idE :: (Monoid e) => e
idE = mempty


{-| Semigroup with an 'absorb' (or "zero") element following this law:

@
a <> absorb = absorb <> a = absorb
@
-}
class (Semigroup a) => Absorbing a where
  absorb :: a

instance Absorbing () where
  absorb = ()

instance (Absorbing a1, Absorbing a2) => Absorbing (a1,a2) where
  absorb = (absorb, absorb)

instance (Absorbing a1, Absorbing a2, Absorbing a3) => Absorbing (a1,a2,a3) where
  absorb = (absorb, absorb, absorb)

instance (Absorbing a1, Absorbing a2, Absorbing a3, Absorbing a4) => Absorbing (a1,a2,a3,a4) where
  absorb = (absorb, absorb, absorb, absorb)

instance (Absorbing a1, Absorbing a2, Absorbing a3, Absorbing a4, Absorbing a5) => Absorbing (a1,a2,a3,a4,a5) where
  absorb = (absorb, absorb, absorb, absorb, absorb)

instance (Absorbing a) => Absorbing (Maybe a) where
  absorb = Just absorb

impl :: (Eq a, Semigroup a) => a -> a -> Bool
impl a b = a <> b == a

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

{-| There are some laws.

@
effectLe (intoCap e) idE e = True

effectLe c1 idE e = True
and
c1 <> intoCap e = intoCap e
implies
c1 = intoCap e
@
-}
class EffectOrd c where
  type Ef c
  -- Preorder on effects.
  effectLe :: c -> Ef c -> Ef c -> Bool
  intoCap :: Ef c -> c

{-| 'Split' is related to the 'Monoid' implementation by the following
    law:

@
'split' a1 a2 = 'Maybe.Just' a3
iff
a1 = a2 'Semigroup.<>' a3
@
-}
class (Eq a, Monoid a) => Split a where
  split :: a -> a -> Maybe a
  split a1 a2 | a1 == a2 = Just mempty
              | a2 == mempty = Just a1
              | otherwise = Nothing

class (EffectDom e s, StateOrd c s, EffectOrd c, Ef c ~ e) => Camo c e s


-- Possibly the "downgrade" function should be provided by the Camo
-- implementation, since it involves both the state (a test on a
-- state) and also the effects (a reduced set of blocked effects).
-- Unless it makes sense to state the reduced set of blocked effects
-- as a Camo instead?

-- instance (EffectDom e s, StateOrd c s, EffectOrd c e)
--   => Camo c (ConstE e s) s

instance Split ()

instance (Eq s) => StateOrd () s where
  stateLe _ s1 s2 = s1 == s2

instance EffectOrd () where
  type Ef () = ()
  effectLe () () () = True
  intoCap () = ()

instance (Eq s) => Camo () () s

instance (StateOrd c1 s1, StateOrd c2 s2) => StateOrd (c1,c2) (s1,s2) where
  stateLe (c1,c2) (a1,a2) (b1,b2) =
    stateLe c1 a1 b1
    && stateLe c2 a2 b2

instance (StateOrd c1 s1, StateOrd c2 s2, StateOrd c3 s3) => StateOrd (c1,c2,c3) (s1,s2,s3) where
  stateLe (c1,c2,c3) (a1,a2,a3) (b1,b2,b3) =
    stateLe c1 a1 b1
    && stateLe c2 a2 b2
    && stateLe c3 a3 b3

instance (StateOrd c1 s1, StateOrd c2 s2, StateOrd c3 s3, StateOrd c4 s4) => StateOrd (c1,c2,c3,c4) (s1,s2,s3,s4) where
  stateLe (c1,c2,c3,c4) (a1,a2,a3,a4) (b1,b2,b3,b4) =
    stateLe c1 a1 b1
    && stateLe c2 a2 b2
    && stateLe c3 a3 b3
    && stateLe c4 a4 b4

instance (EffectOrd c1, EffectOrd c2) => EffectOrd (c1,c2) where
  type Ef (c1,c2) = (Ef c1, Ef c2)
  effectLe (c1,c2) (a1,a2) (b1,b2) =
    effectLe c1 a1 b1
    && effectLe c2 a2 b2
  intoCap (e1,e2) = (intoCap e1, intoCap e2)

-- instance (EffectOrd c1 s1, EffectOrd c2 s2, EffectOrd c3 s3) => EffectOrd (c1,c2,c3) (s1,s2,s3) where
--   effectLe (c1,c2,c3) (a1,a2,a3) (b1,b2,b3) =
--     effectLe c1 a1 b1
--     && effectLe c2 a2 b2
--     && effectLe c3 a3 b3

-- instance (EffectOrd c1 s1, EffectOrd c2 s2, EffectOrd c3 s3, EffectOrd c4 s4) => EffectOrd (c1,c2,c3,c4) (s1,s2,s3,s4) where
--   effectLe (c1,c2,c3,c4) (a1,a2,a3,a4) (b1,b2,b3,b4) =
--     effectLe c1 a1 b1
--     && effectLe c2 a2 b2
--     && effectLe c3 a3 b3
--     && effectLe c4 a4 b4

instance (Camo c1 e1 s1, Camo c2 e2 s2) => Camo (c1,c2) (e1,e2) (s1,s2)

-- instance (Camo c1 e1 s1, Camo c2 e2 s2, Camo c3 e3 s3) => Camo (c1,c2,c3) (e1,e2,e3) (s1,s2,s3)

-- instance (Camo c1 e1 s1, Camo c2 e2 s2, Camo c3 e3 s3, Camo c4 e4 s4) => Camo (c1,c2,c3,c4) (e1,e2,e3,e4) (s1,s2,s3,s4)

-- The universal relation, providing no information, i.e. inconsistent
-- snapshots.
data UniversalC c
  = UniversalC
  | LimitC c
  deriving (Show,Eq,Ord,Generic)


instance (ToJSON c) => ToJSON (UniversalC c) where
  toEncoding = genericToEncoding defaultOptions
instance (ToJSON c, ToJSONKey c) => ToJSONKey (UniversalC c)
instance (FromJSON c) => FromJSON (UniversalC c)
instance (FromJSON c, FromJSONKey c) => FromJSONKey (UniversalC c)

instance (Semigroup c) => Semigroup (UniversalC c) where
  UniversalC <> c = UniversalC
  c <> UniversalC = UniversalC
  LimitC c1 <> LimitC c2 = LimitC (c1 <> c2)

instance (Monoid c) => Monoid (UniversalC c) where
  mempty = LimitC mempty

instance (Meet c) => Meet (UniversalC c) where
  UniversalC `meet` c = c
  c `meet` UniversalC = c
  LimitC c1 `meet` LimitC c2 = LimitC (c1 `meet` c2)

  c <=? UniversalC = True
  UniversalC <=? c = False
  LimitC c1 <=? LimitC c2 = c1 <=? c2

instance (Meet c) => BMeet (UniversalC c) where
  meetId = UniversalC

instance (Split c) => Split (UniversalC c) where
  split UniversalC _ = Just UniversalC
  split _ UniversalC = Nothing
  split (LimitC c1) (LimitC c2) = LimitC <$> split c1 c2

instance (Monoid e, Cap c e) => Cap (UniversalC c) e where
  mincap e = LimitC (mincap e)
  undo e = LimitC (undo e)
  weaken UniversalC _ = Just mempty
  weaken _ UniversalC = Nothing
  weaken (LimitC c1) (LimitC c2) = weaken c1 c2

instance (Absorbing c) => Absorbing (UniversalC c) where
  absorb = LimitC absorb

instance (StateOrd c e) => StateOrd (UniversalC c) e where
  stateLe UniversalC = \_ _ -> True
  stateLe (LimitC c) = stateLe c

instance (EffectOrd c) => EffectOrd (UniversalC c) where
  type Ef (UniversalC c) = Ef c
  effectLe UniversalC = \_ _ -> True
  effectLe (LimitC c) = effectLe c

  intoCap e = LimitC (intoCap e)

instance (Camo c e s) => Camo (UniversalC c) e s

class Meet a where
  meet :: a -> a -> a
  (<=?) :: a -> a -> Bool
  a <=? b | compareP a b == Just LT = True
          | otherwise = False

  compareP :: a -> a -> Maybe Ordering
  compareP a b | a <=? b && b <=? a = Just EQ
               | a <=? b = Just LT
               | b <=? a = Just GT
               | otherwise = Nothing

class (Meet a) => BMeet a where
  meetId :: a

class (BMeet c, Split c, Monoid e) => Cap c e where
  mincap :: e -> c

  undo :: e -> c
  undo _ = mempty

  weaken :: c -> c -> Maybe e
  weaken c1 c2 | meetId <=? c1 = Just idE
               | c2 <=? mempty = Just idE
               | otherwise = Nothing

{-| The identity relation, a synonym for 'mempty'. -}
idC :: (Monoid c) => c
idC = mempty

{-| The universal relation, a synonym for 'meetId'. -}
uniC :: (BMeet c) => c
uniC = meetId
