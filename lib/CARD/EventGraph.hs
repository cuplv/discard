{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module CARD.EventGraph
  ( Edge (Edge,edgeSet)
  , unsafeMakeEdge
  , empty
  , EGB (..)
  , EG (..)
  , append
  , liftEvent
  , contains
  , vis'
  , merge
  , pop
  , edge
  , foldg
  , folds
  , CacheResult (..)
  , serialize
  ) where

import Control.Monad (foldM,filterM)
import Data.Foldable (foldl')
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Trans
import GHC.Generics
import Data.Aeson

import CARD.CvRDT

-- | The "edge set" defining an event graph.  The edge set is the set
-- of "latest" events, or those which do not have any outgoing edges
-- in the graph.  This edge is not a single event because events in
-- the graph are only partially ordered.
--
-- 'e' is the recursive event structure and 'd' is the data payload
-- each event holds.
data Edge r d = Edge { edgeSet :: Set (Event r d) } deriving (Generic)

deriving instance (Eq (Event r d)) => Eq (Edge r d)
deriving instance (Ord (Event r d)) => Ord (Edge r d)

instance (Show (Event r d)) => Show (Edge r d) where
  show g = "{ " 
           ++ concat (map ((++ ", ") . show) (Set.toList (edgeSet g))) 
           ++ " }"

instance (ToJSON (Event r d)) => ToJSON (Edge r d) where
  toEncoding = genericToEncoding defaultOptions

instance (Ord (Event r d), FromJSON (Event r d)) => FromJSON (Edge r d)

-- | Create an empty event graph
empty :: Edge e d
empty = Edge Set.empty

-- | Create an event graph from a set of events without checking for
-- uniqueness.  Note that this allows events to duplicate in
-- serialization!  This function is intended for testing purposes.
unsafeMakeEdge = Edge

class EGB r where
  data Event r :: * -> *

-- type Event r d = (EventStruct r) d

class (EGB r, Monad m, Eq (Event r d), Ord (Event r d)) => EG r d m where
  event :: r -> Edge r d -> d -> m (Event r d)
  unpack :: r -> Event r d -> m (d, Edge r d)
  vis :: r -> Event r d -> Event r d -> m Bool

-- | Create a new event, appending it to the event graph
append :: (EG r d m) => r -> d -> Edge r d -> m (Edge r d)
append r d g = liftEvent <$> event r g d

-- | Create a graph with a single event as its edge
liftEvent :: (Ord (Event r d)) => Event r d -> Edge r d
liftEvent e = Edge . Set.fromList $ [e]

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
contains r (Edge s) e = do let inEdge = Set.member e s 
                           inHist <- orM (vis r e) (Set.toList s)
                           return $ or [inEdge,inHist]

-- | Check if the event is in the /history/ of any of the graph's edge
-- events.  This will return 'False' if the event is actually one of
-- the edge events.
vis' :: (EG r d m) => r -> Event r d -> Edge r d -> m Bool
vis' r e (Edge s) = orM (vis r e) (Set.toList s)

-- | Merge two event graphs (which may have common prefixes)
mergeEG :: (EG r d m) => r -> Edge r d -> Edge r d -> m (Edge r d)
mergeEG r g1@(Edge s1) g2@(Edge s2) = do 
  us1 <- filterM (\e -> not <$> (vis' r e g2)) (Set.toList s1)
  us2 <- filterM (\e -> not <$> (vis' r e g1)) (Set.toList s2)
  return (Edge (Set.union (Set.fromList us1) (Set.fromList us2)))

instance (EG r d m) => CvRDT r (Edge r d) m where
  merge = mergeEG
  cvempty _ = return empty

setLast :: Set a -> Maybe (a, Set a)
setLast xs = let (as,bs) = Set.splitAt (Set.size xs - 1) xs
             in case Set.toList bs of
                  b:[] -> Just (b,as)
                  _ -> Nothing

-- | Remove the (arbitrarily) last event from an event graph,
-- returning its payload and the edge set of the rest of the graph.
pop :: (EG r d m) => r -> Edge r d -> m (Maybe (d, Edge r d))
pop r (Edge s) = mayMap f (setLast s)
  where f (e,es1) = do (d,es2) <- unpack r e
                       es3 <- mergeEG r (Edge es1) es2
                       return (d,es3)

-- | Unpack all events in an edge set, returning them in their
-- arbitrary order sequence
edge :: (EG r d m) => r -> Edge r d -> m [(d, Edge r d)]
edge r (Edge s) = mapM (unpack r) (Set.toList s)

-- | Fold over the elements of an event graph
foldg :: (EG r d m) => r -> (s -> d -> s) -> s -> Edge r d -> m s
foldg r f s g = foldl' f s <$> serialize r g

data CacheResult = Hit | Partial | Miss deriving (Show,Eq,Ord)

folds :: (Ord s, EG r d m) 
      => r 
      -> (d -> s -> s) 
      -> s 
      -> Map (Edge r d) s
      -> Edge r d 
      -> m (s,CacheResult)
folds r f s m g = do 
  case Map.lookup g m of
    (Just smr) -> return (smr,Hit)
    Nothing -> do
     (s',res) <- foldsr r f s m g
     return (s',case res of
                  True -> Partial
                  False -> Miss)

foldsr :: (Ord s, EG r d m) 
       => r 
       -> (d -> s -> s) 
       -> s 
       -> Map (Edge r d) s
       -> Edge r d 
       -> m (s,Bool)
foldsr r f s m g = do 
  -- Try summaries
  case Map.lookup g m of
    Just smr -> return (smr,True)
    Nothing -> do
      -- Are we finished?
      me <- pop r g
      case me of
        Nothing -> return (s,False)
        Just (d,g') -> do
          (s',res) <- foldsr r f s m g'
          return (f d s', res)


-- | Totally order the elements of an event graph, using their
-- arbitrary ordering to resolve parallel events.
serialize :: (EG r d m) => r -> Edge r d -> m [d]
serialize r g = do me <- pop r g
                   case me of
                     Nothing -> return []
                     Just (d,g') -> (++ [d]) <$> serialize r g'
