{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.CARD.Capconf
  ( Capconf
  , localG
  , remoteG
  , remoteG'
  , consumeG
  , transferG
  , acceptG
  , maskG
  , unmaskAllG
  , summarizeG
  ) where

import Data.CARD.Classes
import Data.CvRDT

import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Control.Monad (foldM)
import GHC.Generics

data Change i c
  = Gain c
  | Drop c
  | Mask i c
  | Unmasked
  deriving (Show,Eq,Ord,Generic)

change :: (Meet c, Monoid c, Split c) => c -> Change i c -> Maybe c
change a (Gain b) = Just (a <> b)
change a (Drop b) = split a b
change a (Mask _ b) = Just (a `meet` b)
change a Unmasked = Just a

-- -- | Sequence number says how many changes have been summarized,
-- -- capability is initial capability, and list of changes are changes
-- -- to that capability.
-- type Hist i c = (Int, c, [Change i c], c)

data Hist i c
  = Hist { histInit :: c
         , histSummed :: Int
         , histChanges :: [Change i c]
         , histInbox :: c
         }
  deriving (Show,Eq,Ord,Generic)

takeLonger l1 l2 | length l1 > length l2 = l1
                 | otherwise = l2

mergeH :: (Semigroup c) => Hist i c -> Hist i c -> Hist i c
mergeH (Hist c1 n1 ms1 a1) h2@(Hist c2 n2 ms2 a2) | n1 < n2 =
  mergeH (Hist c2 n2 (take (n2 - n1) ms1) a1) h2
mergeH h1@(Hist c1 n1 ms1 a1) (Hist c2 n2 ms2 a2) | n1 > n2 =
  mergeH h1 (Hist c1 n1 (take (n1 - n2) ms2) a2)
mergeH (Hist c1 n1 ms1 a1) (Hist _ _ ms2 a2) =
  Hist c1 n1 (takeLonger ms1 ms2) (a1 <> a2)

initHist :: (Monoid c) => c -> Hist i c
initHist c = Hist c 0 [] mempty

-- | Add a new capability change to history.  If this is an invalid
-- change, this will generate a runtime error.
newChange :: (Meet c, Monoid c, Split c) 
  => Change i c -> Hist i c -> Hist i c
newChange m@(Mask _ _) (Hist c n ms a) = Hist c n (ms ++ [m]) a
newChange Unmasked _ = error "Can't add a new Unmasked change."
newChange m (Hist c n [] a) = case change c m of
  Just c' -> Hist c' n [] a
  Nothing -> error "Invalid capability newChange."
newChange m (Hist c n ms a) = Hist c n (ms ++ [m]) a

-- | Accept transferred caps from inbox (if any).
acceptH :: (Meet c, Monoid c, Split c) => Hist i c -> Hist i c
acceptH h@(Hist _ _ _ a) = case newChange (Gain a) h of
  Hist c n ms _ -> Hist c n ms mempty

-- | Add caps to inbox.
depositH :: (Semigroup c) => c -> Hist i c -> Hist i c
depositH c1 (Hist c n ms a) = Hist c n ms (a <> c1)

-- | Remove masks held by the @i@ replica.
unmaskMine :: (Eq i) => i -> Hist i c -> Hist i c
unmaskMine i (Hist c n ms a) =
  let f = \case
             Mask j c | j == i -> Unmasked
             m -> m
  in Hist c n (map f ms) a

-- | Remove masks held by replicas other than the @i@ argument.
unmaskOthers :: (Eq i) => i -> Hist i c -> Hist i c
unmaskOthers i (Hist c n ms a) =
  let f = \case
             Mask j _ | j /= i -> Unmasked
             m -> m
  in Hist c n (map f ms) a

-- | Remove non-mask changes from the prefix, by updating the init
-- capability.
summarizeH :: (Meet c, Monoid c, Split c) => Hist i c -> Hist i c
summarizeH h@(Hist _ _ [] _) = h
summarizeH h@(Hist c n (Mask _ _ : ms) _) = h
summarizeH (Hist c n (m:ms) a) = 
  case change c m of
    Just c' -> summarizeH (Hist c' (n + 1) ms a)
    Nothing -> error "Invalid summarizeH."

-- | Compute capability from history.
getCap :: (Meet c, Monoid c, Split c) => Hist i c -> Maybe c
getCap (Hist c _ ms a) = (<> a) <$> foldM change c ms

data Capconf i c
  = Capconf { capConf :: Map i (Hist i c) }
  deriving (Show,Eq,Ord,Generic)

instance 
  (Monad m, Ord i, Ord c, Meet c, Monoid c, Split c)
  => CvRDT r (Capconf i c) m where
  cvempty _ = return $ Capconf mempty
  cvmerge _ (Capconf m1) (Capconf m2) = 
    return $ Capconf $ Map.unionWith mergeH m1 m2

-- | Get capability belonging to @i@ replica.  If the @i@ replica does
-- not have a capability assigned, it is assumed to be
-- 'Monoid.mempty'.
localG :: (Ord i, Meet c, Monoid c, Split c) => i -> Capconf i c -> c
localG i (Capconf m) = case Map.lookup i m of
  Just h -> fromJust $ getCap h
  Nothing -> mempty

-- | Get map of capabilities held by remote replicas (dropping masks
-- imposed by remote replicas).
remoteG :: (Ord i, Meet c, Monoid c, Split c) => i -> Capconf i c -> Map i c
remoteG i (Capconf m) =
  Map.map (fromJust . getCap . unmaskOthers i) . Map.delete i $ m

-- | Get accumulated capability held by all remote replicas (dropping
-- masks imposed by remote replicas).
remoteG' :: (Ord i, Meet c, Monoid c, Split c) => i -> Capconf i c -> c
remoteG' i cf = Map.foldr (<>) mempty (remoteG i cf)

gainG :: (Ord i, Meet c, Monoid c, Split c) => i -> c -> Capconf i c -> Capconf i c
gainG i c (Capconf m) = case Map.lookup i m of
  Just h -> Capconf $ Map.insert i (newChange (Gain c) h) m
  Nothing -> Capconf $ Map.insert i (initHist c) m

dropG :: (Ord i, Meet c, Monoid c, Split c) => i -> c -> Capconf i c -> Maybe (Capconf i c)
dropG i c (Capconf m) =
  if c <=? localG i (Capconf m)
     then Just (Capconf (Map.adjust (newChange (Drop c)) i m))
     else Nothing

depositG :: (Ord i, Meet c, Monoid c, Split c) => (i,c) -> Capconf i c -> Capconf i c
depositG (i2,c) (Capconf m) = Capconf $ Map.adjust (depositH c) i2 m

-- | @transferG i1 (i2,c) g@ transfers the capability @c@ from replica
-- @i1@ to replica @i2@.  This should be applied locally to the
-- configuration at @i1@ and then lazily propogated to @i2@.
transferG :: (Ord i, Meet c, Monoid c, Split c) => i -> (i,c) -> Capconf i c -> Maybe (Capconf i c)
transferG i1 (i2,c) g = depositG (i2,c) <$> dropG i1 c g

acceptG :: (Ord i, Meet c, Monoid c, Split c) => i -> Capconf i c -> Capconf i c
acceptG i (Capconf m) = Capconf $ Map.adjust acceptH i m

-- | Reduce the local capability by using an effect, if possible.
-- This returns 'Maybe.Nothing' if the local capabilty was not
-- sufficient.
consumeG :: (Ord i, Cap c e) => i -> e -> Capconf i c -> Maybe (Capconf i c)
consumeG i e = dropG i (mincap e)

-- | @maskG i1 (i2,c) g@ applies @c@ as a mask to @i1@'s capability,
-- on a request from @i2@.  This function should be used at @i1@.
maskG :: (Ord i, Meet c, Monoid c, Split c) => i -> (i,c) -> Capconf i c -> Capconf i c
maskG i1 (i2,c) (Capconf m) = case Map.lookup i1 m of
  Just h -> Capconf $ Map.adjust (newChange (Mask i2 c)) i1 m
  Nothing -> Capconf m

-- | @unmaskAllG i g@ removes all masks requested by @i@ from all
-- remote capabilities.  This should be used after completing a
-- transaction.
unmaskAllG :: (Ord i, Meet c, Monoid c, Split c) => i -> Capconf i c -> Capconf i c
unmaskAllG i (Capconf m) = Capconf $ Map.map (unmaskMine i) m

summarizeG :: (Ord i, Meet c, Monoid c, Split c) => i -> Capconf i c -> Capconf i c
summarizeG i (Capconf m) = Capconf $ Map.adjust summarizeH i m
