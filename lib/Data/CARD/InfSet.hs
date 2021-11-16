{-# LANGUAGE DeriveGeneric #-}

module Data.CARD.InfSet
  ( InfSet
  , empty
  , singleton
  , universal
  , fromSet
  , fromList
  , member
  , isSubsetOf
  , isEmpty
  , union
  , intersection
  ) where

import Data.CARD.Classes

import Data.Aeson
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics

-- -- | 'Linear' is a class for datatypes which have a multiplicative
-- -- conjunction operator '|*|' and an additive disjunction operator
-- -- '|+|'.  For all capabilities, '|+|' works as set-intersection on
-- -- the set of permitted effects.  For infinite capabilities, '|*|'
-- -- works like set-union.
-- class Linear a where
--   (|*|) :: a -> a -> a
--   (|+|) :: a -> a -> a

-- | @'InfSet' a@ represents a set on@a@ using either an allow-list or
-- a block-list.  This allows it to finitely represent the set of all
-- elements of type @a@.
data InfSet a 
  = Allow (Set a) | Block (Set a)
  deriving (Show,Eq,Ord,Generic)

instance (ToJSON c) => ToJSON (InfSet c) where
  toEncoding = genericToEncoding defaultOptions
instance (ToJSON c, ToJSONKey c) => ToJSONKey (InfSet c)
instance (Ord c, FromJSON c) => FromJSON (InfSet c)
instance (Ord c, FromJSON c, FromJSONKey c) => FromJSONKey (InfSet c)

instance (Ord a) => Semigroup (InfSet a) where
  Allow a1 <> Allow a2 = Allow (Set.union a1 a2)
  Allow a1 <> Block b2 = Block (Set.difference b2 a1)
  Block b1 <> Block b2 = Block (Set.union b1 b2)
  Block b1 <> Allow a2 = Block (Set.difference b1 a2)

instance (Ord a) => Monoid (InfSet a) where
  mempty = empty

instance (Ord a) => Meet (InfSet a) where
  Allow a1 `meet` Allow a2 = Allow (Set.intersection a1 a2)
  Allow a1 `meet` Block b2 = Allow (Set.difference a1 b2)
  Block b1 `meet` Block b2 = Block (Set.intersection b1 b2)
  Block b1 `meet` Allow a2 = Allow (Set.difference a2 b1)

  (<=?) = isSubsetOf

instance (Ord a) => BMeet (InfSet a) where
  meetId = universal

instance (Ord a) => Split (InfSet a) where
  split a b = if b <=? a
                 then Just $ difference a b
                 else Nothing

-- | Set membership for 'IDSet'.
member :: (Ord a) => a -> InfSet a -> Bool
member i (Allow as) = Set.member i as
member i (Block bs) = not $ Set.member i bs

union :: (Ord a) => InfSet a -> InfSet a -> InfSet a
union = (<>)

intersection :: (Ord a) => InfSet a -> InfSet a -> InfSet a
intersection = meet

isSubsetOf :: (Ord a) => InfSet a -> InfSet a -> Bool
isSubsetOf (Allow a1) (Allow a2) = Set.isSubsetOf a1 a2
isSubsetOf (Allow a1) (Block b2) = Set.disjoint a1 b2
isSubsetOf (Block b1) (Block b2) = Set.isSubsetOf b2 b1
-- This last case would only succeed if either Block contained all
-- integers minus Allow, or the Allow contained all integers minus
-- Block.  We assume the 'Data.Set' of all 'Int's cannot be
-- constructed, so we don't check.
isSubsetOf (Block _) (Allow _) = False

difference :: (Ord a) => InfSet a -> InfSet a -> InfSet a
difference a b = case (a,b) of
  (Allow a1, Allow a2) -> Allow $ Set.difference a1 a2
  (Allow a1, Block b2) -> Allow $ Set.intersection a1 b2
  (Block b1, Block b2) -> Block $ Set.difference b2 b1
  (Block b1, Allow a2) -> Block $ Set.union b1 a2

-- | Convert a (necessarily finite) standard @'Set' a@ into an
-- 'InfSet'.
fromSet :: Set a -> InfSet a
fromSet s = Allow s

-- | Attempt to convert an 'InfSet' into a 'Set'.  This only works if
-- the 'InfSet' happens to be finite.
toSet :: InfSet a -> Maybe (Set a)
toSet (Allow s) = Just s
toSet _ = Nothing

-- | Convert a 'List' into an 'InfSet'.
fromList :: (Ord a) => [a] -> InfSet a
fromList = fromSet . Set.fromList

-- | Create an 'InfSet' containing a single element.
singleton :: a -> InfSet a
singleton a = Allow (Set.singleton a)

-- | Create an 'InfSet' containing all elements.  This is the same as
-- 'Data.CARD.Classes.meetId'.
universal :: InfSet a
universal = Block Set.empty

-- | Create an empty 'InfSet'.  This is the same as 'Data.Monoid.mempty'
empty :: InfSet a
empty = Allow Set.empty

isEmpty :: (Eq a) => InfSet a -> Bool
isEmpty (Allow s) | s == Set.empty = True
isEmpty _ = False
