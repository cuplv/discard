{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.EventGraph where

import Control.Monad (foldM)

-- class EGStore s where
--   type EGStore

class Monoid e => Effect e where
  type Store e
  eval :: [e] -> Store e

-- | An 'EventGraph' is a DAG of events.
class (Monad (Resolver s)) => EventGraph s where
  -- | 'EventGraph' instances intended for heavy use may use IO; any
  -- necessary monadic actions can be encapsulated by the 'Resolver'.
  type Resolver s :: * -> *
  -- | Instantiate an empty 'EventGraph' for some type 'e' of events.
  empty :: (Ord e) => s e
  -- | Add an event of type 'e' to the 'EventGraph'
  add :: (Ord e) => e -> s e -> Resolver s (s e)
  -- | Merge two 'EventGraph's, which may share events.
  merge :: (Ord e) => s e -> s e -> Resolver s (s e)
  -- | Examine the "edge-set" of the 'EventGraph'.  The edge-set is
  -- the set of events which do not come before any other event in the
  -- graph.
  --
  -- 'edge' is used to recursively unpack and evaluate the history of
  -- events stored in an 'EventGraph'.
  edge :: (Ord e) => s e -> Resolver s [(e, s e)]

toList :: (EventGraph g, Ord e) => g e -> Resolver g [e]
toList g = do es <- edge g
              case es of
                [] -> return []
                _ -> do g' <- foldM merge empty (map snd es)
                        es' <- toList g'
                        return $ es' ++ (map fst es)
