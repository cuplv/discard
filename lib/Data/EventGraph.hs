{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.EventGraph where

class Monoid e => Effect e where
  type Store e

{- | An 'EventGraph' is a DAG of events. -}
class (Monad (Resolver s)) => EventGraph s where
  -- | 'EventGraph' instances intended for heavy use may use IO; any
  -- necessary monadic actions can be encapsulated by the 'Resolver'.
  type Resolver s :: * -> *
  -- | Instantiate an empty 'EventGraph' for some type 'e' of events.
  empty :: Resolver s (s e)
  -- | Add an event of type 'e' to the 'EventGraph'
  add :: e -> s e -> Resolver s (s e)
  -- | Merge two 'EventGraph's, which may share events.
  merge :: s e -> s e -> Resolver s (s e)
  -- | Examine the "edge-set" of the 'EventGraph'.  The edge-set is
  -- the set of events which do not come before any other event in the
  -- graph.
  --
  -- 'edge' is used to recursively unpack and evaluate the history of
  -- events stored in an 'EventGraph'.
  edge :: s e -> Resolver s [(e, s e)]
