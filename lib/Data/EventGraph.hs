{-# LANGUAGE MultiParamTypeClasses #-}

module Data.EventGraph where

import Control.Monad (foldM)
import Data.Foldable (foldl')

class EG g e where
  -- | Instantiate an empty event graph 'g' for some type 'e' of
  -- events.
  empty :: g e

class (Monad m, EG g e) => EGMonad g e m where
  -- | Add an event to the event graph
  add :: e -> g e -> m (g e)
  -- | Merge two event graphs, which may share events.
  merge :: g e -> g e -> m (g e)
  -- | Examine the "edge set" of the event graph.  The edge set is the
  -- set of events which do not come before any other event in the
  -- graph.
  --
  -- 'edge' is used to recursively unpack and evaluate the history of
  -- events stored in an event graph.
  edge :: g e -> m [(e, g e)]

foldg :: (EGMonad g e m) => (s -> e -> s) -> s -> g e -> m s
foldg f s g = foldl' f s <$> toList g

toList :: (EGMonad g e m) => g e -> m [e]
toList g = do es <- edge g
              case es of
                [] -> return []
                _ -> do g' <- foldM merge empty (map snd es)
                        es' <- toList g'
                        return $ es' ++ (map fst es)
