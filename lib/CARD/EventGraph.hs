module CARD.EventGraph
  ( Edge
  , getEvents
  , empty
  , EGB (..)
  , EG (..)
  , append
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

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable (foldl')

import CARD.EventGraph.Internal

-- | Fold over the elements of an event graph
foldg :: (EG r d m) => r -> (s -> d -> s) -> s -> Edge r d -> m s
foldg r f s g = foldl' f s <$> serialize r g

-- | Report on usage of summaries.
--
-- 'Hit' indicates that the entire graph had a summary, 'Partial'
-- means that a summary was used at some point in the graph, and
-- 'Miss' means that the graph was folded entirely without any
-- opportunity to use a summary.
data CacheResult = Hit | Partial | Miss deriving (Show,Eq,Ord)

-- | Fold using a set of summaries, reporting whether a summary was
-- used.
folds :: (Ord d, Ord s, EG r d m) 
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

foldsr :: (Ord d, Ord s, EG r d m) 
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
