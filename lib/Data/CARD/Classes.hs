{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.CARD.Classes
  ( EffectDom (..)
  , idE
  , ConstE (..)
  , Absorbing (..)
  , StateOrd (..)
  , EffectOrd (..)
  , Camo (..)
  , uniC
  , idC
  , UniversalC (..)
  ) where

import Data.Aeson
import GHC.Generics

{-| An 'EffectDom' is a domain of effects (@e@) on some state type
    (@s@), in which each effect denotes (by 'runEffect') a pure
    function on a state value.  The effects must form a
    'Data.Monoid.Monoid' according to the following laws:

@
\-\- Identity
'runEffect' 'Data.Monoid.mempty' = 'Data.Function.id'

\-\- Composition
'runEffect' (e2 'Data.Semigroup.<>' e1) = 'runEffect' e2 'Data.Function..' 'runEffect' e1
@

    Note that 'Data.Semigroup.<>' composes effects right-to-left, just
    like function composition.
-}
class (Monoid e) => EffectDom e s where
  runEffect :: e -> s -> s

instance EffectDom () s where
  runEffect () = id

instance
  ( EffectDom e1 s1
  , EffectDom e2 s2 )
  => EffectDom (e1,e2) (s1,s2) where
  runEffect (e1,e2) (s1,s2) =
    ( runEffect e1 s1
    , runEffect e2 s2 )

instance
  ( EffectDom e1 s1
  , EffectDom e2 s2
  , EffectDom e3 s3 )
  => EffectDom (e1,e2,e3) (s1,s2,s3) where
  runEffect (e1,e2,e3) (s1,s2,s3) =
    ( runEffect e1 s1
    , runEffect e2 s2
    , runEffect e3 s3 )

instance
  ( EffectDom e1 s1
  , EffectDom e2 s2
  , EffectDom e3 s3
  , EffectDom e4 s4 )
  => EffectDom (e1,e2,e3,e4) (s1,s2,s3,s4) where
  runEffect (e1,e2,e3,e4) (s1,s2,s3,s4) =
    ( runEffect e1 s1
    , runEffect e2 s2
    , runEffect e3 s3
    , runEffect e4 s4 )

{-| The identity effect, a synonym for 'Data.Monoid.mempty'. -}
idE :: (Monoid e) => e
idE = mempty

{-| t'ConstE' wraps an effect domain (@e@), adding an effect that
    replaces the current state with a given value.

@
'runEffect' ('ConstE' s) = 'Data.Function.const' s = (\\_ -> s)
@

    Effects from the wrapped domain can be used with 'ModifyE'.

@
'runEffect' ('ModifyE' e) s = 'runEffect' e s
@
-} 
data ConstE e s
  = ConstE s
  | ModifyE e
  deriving (Show,Eq,Ord,Generic)

instance (EffectDom e s) => Semigroup (ConstE e s) where
  ConstE s <> _ = ConstE s
  ModifyE e2 <> ModifyE e1 = ModifyE (e2 <> e1)
  ModifyE e <> ConstE s = ConstE (runEffect e s)

instance (EffectDom e s) => Monoid (ConstE e s) where
  mempty = ModifyE mempty

instance (EffectDom e s) => EffectDom (ConstE e s) s where
  runEffect (ConstE s) = const s
  runEffect (ModifyE e) = runEffect e

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

class (EffectDom e s, StateOrd c s, EffectOrd c e) => Camo c e s

{-| The identity relation, a synonym for 'absorb'. -}
idC :: (Absorbing c) => c
idC = absorb

{-| The universal relation, a synonym for 'mempty'. -}
uniC :: (Monoid c) => c
uniC = mempty

-- Possibly the "downgrade" function should be provided by the Camo
-- implementation, since it involves both the state (a test on a
-- state) and also the effects (a reduced set of blocked effects).
-- Unless it makes sense to state the reduced set of blocked effects
-- as a Camo instead?

instance (StateOrd c s, EffectOrd c e) => EffectOrd c (ConstE e s) where
  effectLe c (ConstE s1) (ConstE s2) = stateLe c s1 s2
  effectLe c _ (ConstE s) = stateTop c s
  effectLe c (ModifyE e1) (ModifyE e2) = effectLe c e1 e2

instance (EffectDom e s, StateOrd c s, EffectOrd c e)
  => Camo c (ConstE e s) s

instance (Eq s) => StateOrd () s where
  stateLe _ s1 s2 = s1 == s2

instance (Eq e, Monoid e) => EffectOrd () e where
  effectLe _ e1 e2 = e1 == e2
  effectSlice _ e1 e2 | e1 == e2 = Just (e2,mempty)
                      | e1 == mempty = Just (mempty,e2)
                      | otherwise = Nothing
  effectMerge _ e1 e2 | e1 == mempty && e2 == mempty = Just mempty
                      | otherwise = Nothing

instance (Eq e, Monoid e, Eq s, EffectDom e s) => Camo () e s

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

instance (EffectOrd c1 s1, EffectOrd c2 s2) => EffectOrd (c1,c2) (s1,s2) where
  effectLe (c1,c2) (a1,a2) (b1,b2) =
    effectLe c1 a1 b1
    && effectLe c2 a2 b2

instance (EffectOrd c1 s1, EffectOrd c2 s2, EffectOrd c3 s3) => EffectOrd (c1,c2,c3) (s1,s2,s3) where
  effectLe (c1,c2,c3) (a1,a2,a3) (b1,b2,b3) =
    effectLe c1 a1 b1
    && effectLe c2 a2 b2
    && effectLe c3 a3 b3

instance (EffectOrd c1 s1, EffectOrd c2 s2, EffectOrd c3 s3, EffectOrd c4 s4) => EffectOrd (c1,c2,c3,c4) (s1,s2,s3,s4) where
  effectLe (c1,c2,c3,c4) (a1,a2,a3,a4) (b1,b2,b3,b4) =
    effectLe c1 a1 b1
    && effectLe c2 a2 b2
    && effectLe c3 a3 b3
    && effectLe c4 a4 b4

instance (Camo c1 e1 s1, Camo c2 e2 s2) => Camo (c1,c2) (e1,e2) (s1,s2)

instance (Camo c1 e1 s1, Camo c2 e2 s2, Camo c3 e3 s3) => Camo (c1,c2,c3) (e1,e2,e3) (s1,s2,s3)

instance (Camo c1 e1 s1, Camo c2 e2 s2, Camo c3 e3 s3, Camo c4 e4 s4) => Camo (c1,c2,c3,c4) (e1,e2,e3,e4) (s1,s2,s3,s4)

-- The universal relation, providing no information, i.e. inconsistent
-- snapshots.
data UniversalC c
  = UniversalC
  | RelateC c
  deriving (Show,Eq,Ord,Generic)

instance (Semigroup c) => Semigroup (UniversalC c) where
  UniversalC <> c = c
  c <> UniversalC = c
  RelateC c1 <> RelateC c2 = RelateC (c1 <> c2)

instance (Semigroup c) => Monoid (UniversalC c) where
  mempty = UniversalC

instance (Absorbing c) => Absorbing (UniversalC c) where
  absorb = RelateC absorb

instance (StateOrd c e) => StateOrd (UniversalC c) e where
  stateLe UniversalC = \_ _ -> True
  stateLe (RelateC c) = stateLe c

cuwrt :: Wrt (UniversalC c) -> Wrt c
cuwrt Wrt = Wrt

instance (EffectOrd c e) => EffectOrd (UniversalC c) e where
  effectLe UniversalC = \_ _ -> True
  effectLe (RelateC c) = effectLe c
  effectSlice wrt e1 e2 = effectSlice (cuwrt wrt) e1 e2
  effectMerge wrt e1 e2 = effectMerge (cuwrt wrt) e1 e2

instance (Camo c e s) => Camo (UniversalC c) e s
