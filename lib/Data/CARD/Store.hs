{-# LANGUAGE FlexibleContexts #-}

module Data.CARD.Store
  ( Hist
  , evalHist
  , Store
  , caps
  , hist
  , rqs
  , histAppend
  -- * Convenient re-exports
  , module Data.CARD
  , module Data.CARD.Capconf
  ) where

import Data.Map (Map,lookup)
import Control.Lens

import Data.CvRDT
import Data.CARD
import Data.CARD.Capconf

-- | A 'Hist' is a sequence of CARD effects each paired with an 'i'
-- identifier naming the replica responsible for them.  The
-- identifiers, combined with each effect's history, make each element
-- of the sequence unique.
type Hist h i e = h (i,e)

-- | Evaluate a 'Hist' according to the evaluation semantics defined
-- for 's' 'Effect's.  A "summaries" 'Map' from history prefixes to
-- store values can be provided as a shortcut.
evalHist :: (CvChain r h (i,e) m, EffectDom e s)
         => r -- ^ Resolver
         -> s -- ^ Initial store value
         -> Hist h i e -- ^ History to evaluate
         -> Map (Hist h i e) s -- ^ Summaries
         -> m s
evalHist r s0 h0 summs =
  foldlC
    r
    (\s e -> eFun (snd e) s)
    (flip Data.Map.lookup summs)
    s0
    h0

type Store q h i c e = (q, (Capconf i c, Hist h i e))

rqs :: Lens' (Store q h i c e) q
rqs = _1

-- | Lens to the 'Capconf' state
caps :: Lens' (Store q h i c e) (Capconf i c)
caps = _2 . _1

-- | Lens to the history structure
hist :: Lens' (Store q h i c e) (Hist h i e)
hist = _2 . _2

-- | Issue a store effect, tagged with a replica ID.
histAppend
  :: (Ord i, Ord c, Ord e, CvChain r h (i,e) m, Meet c, Monoid c, Split c, CvRDT r q m)
  => i
  -> e
  -> CvRepCmd r (Store q h i c e) k m ()
histAppend i e = do 
  r <- use resolver
  incorpOn' hist $ append r (i,e)
  return ()
