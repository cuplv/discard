{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.EventGraph.Internal
  ( Edge (Multi,Single)
  , getEvents
  , flattenEdge
  , empty
  , EGB (..)
  , EG (..)
  , append
  , contains
  , vis'
  , pop
  , edge

  ) where

import Control.Monad (filterM)
import Data.Foldable (foldrM)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics
import Data.Aeson

import Data.CvRDT

-- | The "edge set" defining an event graph.  The edge set is the set
-- of "latest" events, or those which do not have any outgoing edges
-- in the graph.  This edge is not a single event because events in
-- the graph are only partially ordered.
--
-- 'e' is the recursive event structure and 'd' is the data payload
-- each event holds.
data Edge r d = Multi (Set (Event r d))
              | Single d (Edge r d) (Event r d)
              deriving (Generic)

getEvents :: Edge r d -> Set (Event r d)
getEvents (Multi s) = s
getEvents (Single _ _ e) = Set.singleton e

flattenEdge :: Edge r d -> Edge r d
flattenEdge (Single _ _ e) = Multi (Set.singleton e)
flattenEdge g = g

-- Equality and order are considered only on the "canonical" form of
-- an edge, which is the "Multi s" form.  flattenEdge puts any edge
-- into canonical form.

instance (Eq d, Eq (Event r d)) => Eq (Edge r d) where
  (==) (Multi s1) (Multi s2) = s1 == s2
  (==) g1 g2 = flattenEdge g1 == flattenEdge g2

instance (Ord d, Ord (Event r d)) => Ord (Edge r d) where
  compare (Multi s1) (Multi s2) = compare s1 s2
  compare g1 g2 = compare (flattenEdge g1) (flattenEdge g2)

instance (Show (Event r d)) => Show (Edge r d) where
  show g = case g of
    Multi s -> "{ " 
               ++ concat (map ((++ ", ") . show) (Set.toList s)) 
               ++ " }"
    Single _ _ e -> "{ " 
                    ++ concat (map ((++ ", ") . show) [e]) 
                    ++ " }"

instance (ToJSON d, ToJSON (Event r d)) => ToJSON (Edge r d) where
  toEncoding = genericToEncoding defaultOptions

instance (Ord d, FromJSON d, Ord (Event r d), FromJSON (Event r d)) => FromJSON (Edge r d)

-- | Create an empty event graph
empty :: Edge e d
empty = Multi Set.empty

class EGB r where
  data Event r :: * -> *

-- type Event r d = (EventStruct r) d

class (EGB r, Monad m, Eq (Event r d), Ord (Event r d), Ord d) => EG r d m where
  event :: r -> Edge r d -> d -> m (Event r d)
  unpack :: r -> Event r d -> m (d, Edge r d)
  vis :: r -> Event r d -> Event r d -> m Bool

  -- correct implementations remove all elements from the first set
  -- that are in the second set's elements' histories
  multiVis :: r -> Set (Event r d) -> Set (Event r d) -> m (Set (Event r d))
  multiVis r s1 s2 = filterSetM (\e -> not <$> vis' r e (Multi s2)) s1

-- | Create a new event, appending it to the event graph
appendEG :: (EG r d m) => r -> d -> Edge r d -> m (Edge r d)
appendEG r d g = flattenEdge . Single d g <$> event r g d

-- | Create a graph with a single event as its edge
liftEvent :: (Ord (Event r d)) => Event r d -> Edge r d
liftEvent e = Multi . Set.fromList $ [e]

listLast :: [a] -> Maybe a
listLast = \case
  [] -> Nothing
  (a:[]) -> Just a
  (_:as) -> listLast as

mayMap :: (Applicative m) => (a -> m b) -> Maybe a -> m (Maybe b)
mayMap f = \case Just a -> Just <$> f a
                 Nothing -> pure Nothing

orM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
orM f (a:as) = do r <- f a
                  if r
                     then return True
                     else orM f as
orM f [] = return False

-- | Check if the graph contains the event
contains :: (EG r d m) => r -> Edge r d -> Event r d -> m Bool
contains r (Multi s) e = 
  if Set.member e s
     then return True
     else orM (vis r e) (Set.toList s)
contains r (Single d g e1) e2 = 
  if e1 == e2
     then return True
     else contains r g e2

-- | Check if the event is in the /history/ of any of the graph's edge
-- events.  This will return 'False' if the event is actually one of
-- the edge events.
vis' :: (EG r d m) => r -> Event r d -> Edge r d -> m Bool
vis' r e (Multi s) = orM (vis r e) (Set.toList s)
vis' r e1 (Single d g e2) = contains r g e1

-- | This is 'filterM' for a Set
filterSetM :: (Ord a, Monad m) => (a -> m Bool) -> Set a -> m (Set a)
filterSetM f s = foldrM filt Set.empty s
  where filt e s = do r <- f e
                      if r
                         then return (Set.insert e s)
                         else return s

-- | Merge two event graphs which may have common prefixes.
--
-- From an efficiency standpoint, it's important to note that 'merge2'
-- first checks whether the first 'Edge' argument is contained in the
-- second.  So if there is a use case in which one edge @b@ is very
-- likely to contain edge @a@, you should invoke it as @merge2 r a b@.
merge2 :: (EG r d m) => r -> Edge r d -> Edge r d -> m (Edge r d)
merge2 r (Multi s1) (Multi s2) = do
  let -- Sort shared and unshared head events
      shared = Set.intersection s1 s2
      s1' = s1 Set.\\ shared
      s2' = s2 Set.\\ shared
  -- Eliminate s1 unshared heads that are in s2
  s1'' <- multiVis r s1' (s2' <> shared)
  -- Eliminate s2 unshared heads that are in the remains of s1
  s2'' <- multiVis r s2' (s1'' <> shared)
  return (Multi (shared <> s1'' <> s2''))
merge2 r g1 g2@(Single _ g2' _) = 
  if g1 == g2 || g1 == g2'
     then return g2
     else merge2 r g1 (flattenEdge g2)
merge2 r g1@(Single _ g1' _) g2 = 
  if g2 == g1 || g2 == g1'
     then return g1
     else merge2 r (flattenEdge g1) g2

instance (EG r d m) => CvRDT r (Edge r d) m where
  cvmerge = merge2
  cvempty _ = return empty

setLast :: Set a -> Maybe (a, Set a)
setLast xs = let (as,bs) = Set.splitAt (Set.size xs - 1) xs
             in case Set.toList bs of
                  b:[] -> Just (b,as)
                  _ -> Nothing

-- | Remove the (arbitrarily) last event from an event graph,
-- returning its payload and the edge set of the rest of the graph.
popEG :: (EG r d m) => r -> Edge r d -> m (Maybe (d, Edge r d))
popEG r (Multi s) = mayMap f (setLast s)
  where f (e,es1) = do (d,es2) <- unpack r e
                       es3 <- merge2 r (Multi es1) es2
                       return (d,es3)
popEG r (Single d g _) = return $ Just (d,g)

instance (EG r d m) => CvChain r (Edge r) d m where
  pop = popEG
  append = appendEG

-- | Unpack all events in an edge set, returning them in their
-- arbitrary order sequence
edge :: (EG r d m) => r -> Edge r d -> m [(d, Edge r d)]
edge r (Multi s) = mapM (unpack r) (Set.toList s)
edge r (Single d g _) = return [(d,g)]

