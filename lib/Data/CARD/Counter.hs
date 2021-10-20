{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.CARD.Counter 
  ( CounterE
  , addE
  , subE
  , mulE
  , CounterC
  , lowerBound
  , upperBound
  ) where

import GHC.Generics

import Data.CARD.Classes

{-| 'CounterE' provides addition, subtraction, and (positive)
  multiplication effects on 'Num' values.
-}
type CounterE n = ConstE (AddMul n) n

data AddMul n
  = AddMul { mulAmt :: n
           , addAmt :: n
           }
  deriving (Show,Eq,Ord,Generic)

instance (Num n) => Semigroup (AddMul n) where
  AddMul m2 a2 <> AddMul m1 a1 =
    -- Distributing multiplication over addition.
    AddMul (m1 + m2) (a1 * m2 + a2)

mulId :: (Num n) => n
mulId = 1

addId :: (Num n) => n
addId = 0

instance (Num n) => Monoid (AddMul n) where
  mempty = AddMul mulId addId

instance (Num n) => EffectDom (AddMul n) n where
  runEffect (AddMul m a) s = s * m + a

{-| Add @n@ to the state, where @n >= 0@.  A negative @n@ will produce a
  generate a runtime error.

@
'runEffect' ('addE' 1) 2 = 3

'runEffect' ('addE' 0) = 'Data.Function.id'
@
-}
addE :: (Ord n, Num n) => n -> CounterE n
addE n | n >= 0 = ModifyE (AddMul mulId n)
       | otherwise = error $ "Negative value applied to addE."

{-| Subtract @n@ from the state, where @n >= 0@.

@
'runEffect' ('subE' 1) 3 = 2

'runEffect' ('subE' 0) = 'Data.Function.id'
@
-}
subE :: (Ord n, Num n) => n -> CounterE n
subE n | n >= 0 = ModifyE (AddMul mulId (-n))
       | otherwise = error $ "Negative value applied to subE."

{-| Multiply the state by @n@, where @n >= 0@.

@
'runEffect' ('mulE' 2) 3 = 6

'runEffect' ('mulE' 1) = 'Data.Function.id'
@
-}
mulE :: (Ord n, Num n) => n -> CounterE n
mulE n | n >= 0 = ModifyE (AddMul n addId)
       | otherwise = error $ "Negative value applied to mulE."

{-| Check that an 'EAddMul' effect only adds/subtracts, and does not
    multiply.

@
'additive' ('addE' 1) = 'True'

'additive' ('mulE' 2 'Data.Semigroup.<>' 'addE' 1) = 'False'

'additive' ('mulE' 1 'Data.Semigroup.<>' 'addE' 1) = 'True'
@
-}
additive :: (Eq n, Num n) => AddMul n -> Bool
additive (AddMul m _) = m == mulId

data Bounds
  = LowerBound
  | UpperBound
  | ExactValue

instance Semigroup Bounds where
  ExactValue <> _ = ExactValue
  _ <> ExactValue = ExactValue
  LowerBound <> UpperBound = ExactValue
  UpperBound <> LowerBound = ExactValue
  a <> _ = a

instance Absorbing Bounds where
  absorb = ExactValue

instance (Num n, Ord n) => StateOrd Bounds n where
  stateLe ExactValue = (==) -- EQV
  stateLe LowerBound = (<=) -- LEQ
  stateLe UpperBound = (>=) -- GEQ

instance (Num n, Ord n) => EffectOrd Bounds (AddMul n) where
  effectLe c e1 e2 =
    additive e1
    && additive e2
    && stateLe c (addAmt e1) (addAmt e2)

instance (Num n, Ord n) => Camo Bounds (AddMul n) n

type CounterC n = UniversalC Bounds

lowerBound :: CounterC n
lowerBound = RelateC LowerBound

upperBound :: CounterC n
upperBound = RelateC UpperBound
