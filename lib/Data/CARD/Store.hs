{-# LANGUAGE FlexibleContexts #-}

module Data.CARD.Store
  ( Hist
  , evalHist
  , Store
  , locks
  , hist
  , ress
  , getRess
  , histAppend
  -- * Convenient re-exports
  , module Data.CARD
  , module Data.CARD.Locks
  , module Data.CARD.Res
  ) where

import Data.Map (Map,lookup)
import Control.Lens

import Data.CvRDT
import Data.CARD
import Data.CARD.Locks
import Data.CARD.Res

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

-- | A 'Store' for a 'CARD' 's' is a 'CvRDT' pair consisting of a
-- distributed lock state (the 'Locks'), a reservation store (the
-- 'Ress') and an event history (the 'Hist').
type Store h i c e = ((Locks i c, Hist h i e), Ress i e)

-- | Lens to the lock state
locks :: Lens' (Store h i c e) (Locks i c)
locks = _1 . _1

-- | Lens to the history structure
hist :: Lens' (Store h i c e) (Hist h i e)
hist = _1 . _2

-- | Lens to the reservation store
ress :: Lens' (Store h i c e) (Ress i e)
ress = _2

getRess :: Store h i c e -> Ress i e
getRess = snd

-- | Emit a store effect, tagged with a replica ID.
histAppend :: (Ord i, Ord c, Ord e, CvChain r h (i,e) m, Semigroup c)
           => i
           -> e
           -> CvRepCmd r (Store h i c e) k m ()
histAppend i e = do 
  r <- use resolver
  emitOn' hist $ append r (i,e)
  return ()
