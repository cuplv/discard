{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module CARD.EventGraph where

import Control.Monad (foldM)
import Control.Monad.Trans
import Data.Foldable (foldl')

class EventGraph g e where
  -- | Instantiate an empty event graph 'g' for some type 'e' of
  -- events.
  empty :: g e

class (Monad m, EventGraph g e) => MonadEG g e m where
  -- | Add an event to the event graph
  append :: e -> g e -> m (g e)
  -- | Merge two event graphs, which may share events.
  merge :: g e -> g e -> m (g e)
  -- | Examine the "edge set" of the event graph.  The edge set is the
  -- set of events which do not come before any other event in the
  -- graph.
  --
  -- 'edge' is used to recursively unpack and evaluate the history of
  -- events stored in an event graph.
  edge :: g e -> m [(e, g e)]
  -- | Take only the (arbitrarily) last event from the event graph.
  -- 'Nothing' is returned when the graph is empty
  pop :: g e -> m (Maybe e, g e)

instance (Monad (t m), MonadTrans t, MonadEG g e m) => MonadEG g e (t m) where
  append e g = lift $ append e g
  merge g1 g2 = lift $ merge g1 g2
  edge g = lift $ edge g
  pop g = lift $ pop g

foldg :: (MonadEG g e m) => (s -> e -> s) -> s -> g e -> m s
foldg f s g = foldl' f s <$> serialize g

serialize :: (MonadEG g e m) => g e -> m [e]
-- serialize g = do es <- edge g
--                  case es of
--                    [] -> return []
--                    _ -> do g' <- foldM merge empty (map snd es)
--                            es' <- serialize g'
--                            return $ es' ++ (map fst es)
serialize g = do (me,g') <- pop g
                 case me of
                   Nothing -> return []
                   Just e -> (++ [e]) <$> serialize g'
