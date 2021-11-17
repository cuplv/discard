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
  , isAddE
  , isSubE
  , CounterC
  , addC
  , subC
  , mulC
  , lowerBound
  , upperBound
  , Bounds
  , AddMul
  , lowerBound'
  , upperBound'
  ) where

import Data.Aeson
import GHC.Generics

import Data.CARD.Classes
import Data.CARD.Const

{-| 'CounterE' provides addition, subtraction, and (positive)
  multiplication effects on 'Num' values.
-}
type CounterE n = ConstE (AddMul n) n

data AddMul n
  = AddMul { mulAmt :: n
           , addAmt :: n
           }
  deriving (Show,Eq,Ord,Generic)

instance (ToJSON n) => ToJSON (AddMul n) where
  toEncoding = genericToEncoding defaultOptions
instance (FromJSON n) => FromJSON (AddMul n)

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

instance (Ord n, Num n) => EffectSlice (AddMul n) where
  effectSlice (AddMul m1 a1) (AddMul m2 a2) 
    | m1 == mulId && m2 == mulId
      && ((a1 >= 0 && a1 <= a2)
          || (a1 <= 0 && a1 >= a2)) = Just (AddMul m1 a1,AddMul m2 (a2 - a1))
    | otherwise = Nothing
  effectMerge (AddMul m1 a1) (AddMul m2 a2)
    | m1 == mulId && m2 == mulId
      && ((a1 >= 0 && a2 >= 0) 
          || (a1 <= 0 && a2 <= 0)) = Just (AddMul m1 (a1 + a2))
    | otherwise = Nothing


instance (Num n) => EffectDom (AddMul n) n where
  eFun (AddMul m a) s = s * m + a

{-| Add @n@ to the state, where @n >= 0@.  A negative @n@ will produce a
  generate a runtime error.

@
'eFun' ('addE' 1) 2 = 3

'eFun' ('addE' 0) = 'Data.Function.id'
@
-}
addE :: (Ord n, Num n) => n -> CounterE n
addE n | n >= 0 = ModifyE (AddMul mulId n)
       | otherwise = error $ "Negative value applied to addE."

{-| Subtract @n@ from the state, where @n >= 0@.

@
'eFun' ('subE' 1) 3 = 2

'eFun' ('subE' 0) = 'Data.Function.id'
@
-}
subE :: (Ord n, Num n) => n -> CounterE n
subE n | n >= 0 = ModifyE (AddMul mulId (-n))
       | otherwise = error $ "Negative value applied to subE."

{-| Multiply the state by @n@, where @n >= 0@.

@
'eFun' ('mulE' 2) 3 = 6

'eFun' ('mulE' 1) = 'Data.Function.id'
@
-}
mulE :: (Ord n, Num n) => n -> CounterE n
mulE n | n >= 0 = ModifyE (AddMul n addId)
       | otherwise = error $ "Negative value applied to mulE."

isAddE :: (Ord n, Num n) => CounterE n -> Bool
isAddE (ModifyE (AddMul m a)) = m == mulId && a > addId
isAddE _ = False

isSubE :: (Ord n, Num n) => CounterE n -> Bool
isSubE (ModifyE (AddMul m a)) = m == mulId && a < addId
isSubE _ = False

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


-- Nothing for each field means unlimited use, while Just
-- <addId|mulId> means no use.
data Bounds n
  = Bounds { addBound :: Maybe n
           , subBound :: Maybe n
           , mulBound :: Maybe n
           }
  deriving (Eq,Ord,Generic)

showBound :: (Show n) => Maybe n -> String
showBound Nothing = "∞"
showBound (Just n) = show n

instance (Ord n, Num n, Show n) => Show (Bounds n) where
  show b | uniC <=? b = "uniC"
  show b | b <=? idC = "idC"
  show (Bounds a s m) = 
    (if not $ a <=?? Just addId
        then "[+" ++ showBound a ++ "]"
        else "")
    ++ (if not $ s <=?? Just addId
           then "[-" ++ showBound s ++ "]"
           else "")
    ++ (if not $ m <=?? Just mulId
           then "[×" ++ showBound m ++ "]"
           else "")

instance (ToJSON n) => ToJSON (Bounds n) where
  toEncoding = genericToEncoding defaultOptions
instance (ToJSON n, ToJSONKey n) => ToJSONKey (Bounds n)
instance (FromJSON n) => FromJSON (Bounds n)
instance (FromJSON n, FromJSONKey n) => FromJSONKey (Bounds n)

addC' :: (Num n, Ord n) => n -> Bounds n
addC' n | n >= 0 = Bounds (Just n) (Just addId) (Just mulId)
        | otherwise = error $ "Negative value applied to addC."

subC' :: (Num n, Ord n) => n -> Bounds n
subC' n | n >= 0 = Bounds (Just addId) (Just n) (Just mulId)
        | otherwise = error $ "Negative value applied to subC."

mulC' :: (Num n, Ord n) => n -> Bounds n
mulC' n | n >= 0 = Bounds (Just addId) (Just addId) (Just n)
        | otherwise = error $ "Negative value applied to mulC."

lowerBound' :: (Num n) => Bounds n
lowerBound' = Bounds Nothing (Just addId) (Just mulId)

upperBound' :: (Num n) => Bounds n
upperBound' = Bounds (Just addId) Nothing (Just mulId)


-- data Bounds
--   = LowerBound
--   | UpperBound
--   | ExactValue
--   deriving (Show,Eq,Ord,Generic)

(+?) :: (Num n) => Maybe n -> Maybe n -> Maybe n
(+?) a b = (+) <$> a <*> b

(*?) :: (Num n) => Maybe n -> Maybe n -> Maybe n
(*?) a b = (*) <$> a <*> b

instance (Num n) => Semigroup (Bounds n) where
  Bounds a1 s1 m1 <> Bounds a2 s2 m2 =
    Bounds (a1 +? a2) (s1 +? s2) (m1 *? m2)

instance (Num n) => Monoid (Bounds n) where
  mempty = Bounds (Just addId) (Just addId) (Just mulId)

(#?) :: (Num n, Ord n) => Maybe n -> Maybe n -> Maybe n
(#?) (Just a) (Just b) = Just (min a b)
(#?) Nothing b = b
(#?) a Nothing = a

(<=??) :: (Num n, Ord n) => Maybe n -> Maybe n -> Bool
(<=??) (Just a) (Just b) = a <= b
(<=??) _ Nothing = True
(<=??) Nothing _ = False

instance (Num n, Ord n) => Meet (Bounds n) where
  meet (Bounds a1 s1 m1) (Bounds a2 s2 m2) =
    Bounds (a1 #? a2) (s1 #? s2) (m1 #? m2)
  (<=?) (Bounds a1 s1 m1) (Bounds a2 s2 m2) =
    a1 <=?? a2 && s1 <=?? s2 && m1 <=?? m2

instance (Num n, Ord n) => BMeet (Bounds n) where
  meetId = Bounds Nothing Nothing Nothing

instance (Num n, Ord n) => Split (Bounds n) where
  split (Bounds a1 s1 m1) (Bounds a2 s2 m2) =
    let f (Just x) (Just y) | x >= y = Just $ Just $ x - y
                            | otherwise = Nothing
        f Nothing _ = Just Nothing
        f _ Nothing = Nothing

        f2 (Just x) (Just y) | x >= y = Just $ Just $ mulId
                             | otherwise = Nothing
        f2 Nothing _ = Just Nothing
        f2 _ Nothing = Nothing
    in Bounds <$> f a1 a2 <*> f s1 s2 <*> f2 m1 m2

instance (Num n, Ord n) => Cap (Bounds n) (AddMul n) where
  mincap e = if addAmt e >= addId
                then Bounds (Just $ addAmt e) (Just addId) (Just $ mulAmt e)
                else Bounds (Just addId)
                            (Just $ negate (addAmt e))
                            (Just $ mulAmt e)

  undo e = if addAmt e >= addId
              then Bounds (Just addId) (Just $ addAmt e) (Just mulId)
              else Bounds (Just $ negate (addAmt e)) (Just addId) (Just mulId)

  weaken c1@(Bounds a1 s1 m1) c2@(Bounds a2 s2 m2)
    | uniC <=? c1 || c2 <=? idE = Just idE
    | m2 == Just mulId = case (a1,s1) of
        (Just _,Just _) -> Nothing
        (Just a1,Nothing) -> case a2 of
          Just a2 -> Just $ AddMul mulId a2
          Nothing -> Nothing
        (Nothing,Just s1) -> case s2 of
          Just s2 -> Just $ AddMul mulId (-s2)
          Nothing -> Nothing
        (Nothing,Nothing) -> Just idE
    | otherwise = Nothing

type CounterC n = ConstC (Bounds n) n

addC :: (Num n, Ord n) => n -> CounterC n
addC = modifyC . addC'

subC :: (Num n, Ord n) => n -> CounterC n
subC = modifyC . subC'

mulC :: (Num n, Ord n) => n -> CounterC n
mulC = modifyC . mulC'

lowerBound :: (Num n) => CounterC n
lowerBound = modifyC $ lowerBound'

upperBound :: (Num n) => CounterC n
upperBound = modifyC $ upperBound'
