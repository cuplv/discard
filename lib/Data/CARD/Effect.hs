{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.CARD.Effect
  (
  -- * The effect domain class
    EffectDom (..)
  , idEffect
  -- * Concrete effect domains
  , ECounter
  , addAmt
  , mulAmt
  , addE
  , subE
  , mulE
  , additive
  -- * Effect domain combinators
  , EIdentity
  , EConst (..)
  , EOnFunctor (..)
  , EProduct
  , EProduct3
  , EProduct4
  ) where

import Lens.Micro.TH

-- The "mempty" of an effect domain's monoid instance should always
-- represent the identity effect, such that runEffect mempty = id.
class (Monoid e) => EffectDom e s where
  runEffect :: e -> s -> s

idEffect :: (Monoid e) => e
idEffect = mempty

-- The domain containing only the identity effect.
type EIdentity = ()

instance EffectDom () s where
  runEffect () = id

-- Domain providing the Const, or set-new-value effect.
data EConst e s
  = EConst s
  | EModify e

instance (EffectDom e s) => Semigroup (EConst e s) where
  _ <> EConst s = EConst s
  EModify e1 <> EModify e2 = EModify (e1 <> e2)
  EConst s <> EModify e = EConst (runEffect e s)

instance (EffectDom e s) => Monoid (EConst e s) where
  mempty = EModify mempty

instance (EffectDom e s) => EffectDom (EConst e s) s where
  runEffect (EConst s) = const s
  runEffect (EModify e) = runEffect e

newtype EOnFunctor e = EOnFunctor e deriving (Semigroup, Monoid)

instance (Functor m, EffectDom e s) => EffectDom (EOnFunctor e) (m s) where
  runEffect (EOnFunctor e) = fmap (runEffect e)

-- Effects must be monoids... then how do we define effect
-- combinators?  Well, Identity doesn't need to combine with anything.
-- And the Set effect simply obliterates whatever it combines with!

-- And maybe following from calling the no-op effect "Identity", the
-- set effect should be called "Const".

-- A product of two effect domains, serving as an effect domain on
-- products of their states.
type EProduct e1 e2 = (e1,e2)

type EProduct3 e1 e2 e3 = (e1,e2,e3)

type EProduct4 e1 e2 e3 e4 = (e1,e2,e3,e4)

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

data ECounter n = ECounter { _mulAmt :: n, _addAmt :: n }

makeLenses ''ECounter

instance (Num n) => Semigroup (ECounter n) where
  ECounter m1 a1 <> ECounter m2 a2 =
    -- Distributing multiplication over addition.
    ECounter (m1 + m2) (a1 * m2 + a2)

mulId :: (Num n) => n
mulId = 1

addId :: (Num n) => n
addId = 0

instance (Num n) => Monoid (ECounter n) where
  mempty = ECounter mulId addId

instance (Num n) => EffectDom (ECounter n) n where
  runEffect (ECounter m a) s = s * m + a

-- | An ECounter effect that adds.  Only accepts arguments >= 0.
addE :: (Ord n, Num n) => n -> ECounter n
addE n | n >= 0 = ECounter mulId n

-- | An ECounter effect that subtracts.  Only accepts arguments >= 0.
subE :: (Ord n, Num n) => n -> ECounter n
subE n | n >= 0 = ECounter mulId (-n)

-- | An ECounter effect that multiplies.  Only accepts arguments >= 0.
mulE :: (Ord n, Num n) => n -> ECounter n
mulE n | n >= 0 = ECounter n addId

-- | Check that an ECounter effect only adds/subtracts, and does not
-- multiply.
additive :: (Eq n, Num n) => ECounter n -> Bool
additive (ECounter m _) = m == mulId
