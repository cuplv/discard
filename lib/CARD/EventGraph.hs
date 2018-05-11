{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module CARD.EventGraph where

import Control.Monad (foldM)
import Control.Monad.Trans
import Data.Foldable (foldl')
import qualified Data.List as List

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
  pop :: g e -> m (Maybe (e, g e))

instance (Monad (t m), MonadTrans t, MonadEG g e m) => MonadEG g e (t m) where
  append e g = lift $ append e g
  merge g1 g2 = lift $ merge g1 g2
  edge g = lift $ edge g
  pop g = lift $ pop g

foldg :: (MonadEG g e m) => (s -> e -> s) -> s -> g e -> m s
foldg f s g = foldl' f s <$> serialize g

serialize :: (MonadEG g e m) => g e -> m [e]
serialize g = do me <- pop g
                 case me of
                   Nothing -> return []
                   Just (e,g') -> (++ [e]) <$> serialize g'

-- | A zipper for event graphs
--
-- This design won't work!  I need to redefine it such that the focus
-- is a pair of the effect as stored on the left and the effect as
-- stored on the right.  The reason is that when pulling effects up
-- into the edge, they be in the left-stored form so that they can be
-- properly ordered.
--
-- The result is that the orderings must always respect the left side
-- over the right.
data EGZ g e h = EGZ { egzLeft :: [(g e, e, h e)]
                     , egzFocus :: (g e, e, h e)
                     , egzRight :: [(g e, e, h e)]}

maybeTail :: [a] -> Maybe a
maybeTail as = case as of
                 [] -> Nothing
                 a:[] -> Just a
                 _:as' -> maybeTail as'

dropTail :: [a] -> [a]
dropTail as = case as of
                [] -> []
                a:[] -> []
                a:as -> a : dropTail as

initEGZ :: (MonadEG g e m, MonadEG h e m)
        => g e 
        -> m (Maybe (EGZ g e h))
initEGZ g = do es <- edge g
               let es' = map (\(e,g) -> (g,e,empty)) es
               return (case maybeTail es' of
                         Just e -> Just $ EGZ (dropTail es') e []
                         Nothing -> Nothing)

shiftL :: (MonadEG g e m, MonadEG h e m)
       => EGZ g e h
       -> m (Maybe (EGZ g e h))
shiftL (EGZ (e1:es1) (g,e,h) es2) = undefined

shiftR :: (MonadEG g e m, MonadEG h e m)
       => EGZ g e h
       -> m (Maybe (EGZ g e h))
shiftR = undefined

------------------------------------------------------------------------
