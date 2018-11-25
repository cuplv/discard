{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Data.CvRDT
  ( CvRDT (..)
  -- * Core replica logic for hosting a CvRDT
  , CvRepCmd
  , runCvRep
  , check
  , incorp
  , emit
  , emitOn
  , bcast
  , checkMeta
  , resolver
  -- * Extra convenience commands
  , emitFst
  , emitSnd
  , emitFstOn
  , emitSndOn
  -- * CvRDT sub-classes
  , CvChain (..)
  , foldlC
  ) where

import Control.Monad.State
import Data.Foldable (foldlM)

class (Ord s, Monad m) => CvRDT r s m where
  cvmerge :: r -> s -> s -> m s
  cvempty :: r -> m s

instance (CvRDT r1 s1 m, CvRDT r2 s2 m) => CvRDT (r1,r2) (s1,s2) m where
  cvmerge (r1,r2) (s1,s2) (s3,s4) = (,) <$> cvmerge r1 s1 s3 <*> cvmerge r2 s2 s4
  cvempty (r1,r2) = (,) <$> cvempty r1 <*> cvempty r2

data CvReplica r s k m = CvReplica
  { cvrState :: s
  , cvrMetaState :: k
  , cvrResolver :: r
  , cvrOnUpdate :: s -> k -> m k
  , cvrBroadcast :: s -> m () }

type CvRepCmd r s k m = StateT (CvReplica r s k m) m

runCvRep :: (CvRDT r s m)
         => r -- ^ Resolver
         -> k -- ^ Initial meta-state
         -> (s -> m ()) -- ^ Broadcast action
         -> (s -> k -> m k) -- ^ On-update action
         -> CvRepCmd r s k m a -- ^ Command script
         -> m a
runCvRep r k bc ou cmd = do 
  s0 <- cvempty r
  fst <$> runStateT cmd (CvReplica s0 k r ou bc)

-- | Broadcast the current state.
bcast :: (CvRDT r s m) => CvRepCmd r s k m ()
bcast = do s <- cvrState <$> get
           bf <- cvrBroadcast <$> get
           lift $ bf s

-- | Get the current state.
check :: (Monad m) => CvRepCmd r s k m s
check = cvrState <$> get

-- | Get the state resolver.
resolver :: (CvRDT r s m) => CvRepCmd r s k m r
resolver = cvrResolver <$> get

-- | Get the current meta-state.
checkMeta :: (CvRDT r s m) => CvRepCmd r s k m k
checkMeta = cvrMetaState <$> get

-- | Rerun the meta update action on the current state.
updateMeta :: (CvRDT r s m) => CvRepCmd r s k m ()
updateMeta = do 
  rep <- get
  k' <- lift$ (cvrOnUpdate rep) (cvrState rep) (cvrMetaState rep)
  put $ rep { cvrMetaState = k' }

-- | Merge a state into the replica's current state.  The return value
-- is the new combined state, or 'Nothing' if the merge made no
-- change.
incorp :: (CvRDT r s m) => s -> CvRepCmd r s k m (Maybe s)
incorp s2 = do 
  rep <- get
  s3 <- lift$ cvmerge (cvrResolver rep) (cvrState rep) s2
  if s3 /= cvrState rep
     then do put $ rep { cvrState = s3 }
             updateMeta
             return (Just s3)
     else return Nothing

-- | Merge a new state into the replica's current state and broadcast
-- the new current state (only if it has changed).  The return value
-- is the new current state.
emit :: (CvRDT r s m) => s -> CvRepCmd r s k m s
emit s = incorp s >>= \case
  Just s' -> bcast >> return s'
  Nothing -> check

-- | 'emit' an update to only the first component of a pair-state.
emitFst :: (CvRDT r1 s1 m, CvRDT r2 s2 m) 
        => s1 
        -> CvRepCmd (r1,r2) (s1,s2) k m s1
emitFst s1 = do 
  res <- snd . cvrResolver <$> get
  s2 <- lift$ cvempty res
  fst <$> emit (s1,s2)

-- | 'emit' an update to only the second component of a pair-state.
emitSnd :: (CvRDT r1 s1 m, CvRDT r2 s2 m) 
        => s2 
        -> CvRepCmd (r1,r2) (s1,s2) k m s2
emitSnd s2 = do 
  res <- fst . cvrResolver <$> get
  s1 <- lift$ cvempty res
  snd <$> emit (s1,s2)

-- | Run an action on the current state, incorporating and emitting
-- its result as the new current state.  The return value is the new
-- current state.
emitOn :: (CvRDT r s m) => (s -> m s) -> CvRepCmd r s k m s
emitOn f = emit =<< lift . f =<< check

-- | 'emitOn' for only the first component of a pair-state.
emitFstOn :: (CvRDT r1 s1 m, CvRDT r2 s2 m)
          => (s1 -> m s1) 
          -> CvRepCmd (r1,r2) (s1,s2) k m s1
emitFstOn f = emitFst =<< lift . f =<< fst <$> check

-- | 'emitOn' for only the second component of a pair-state.
emitSndOn :: (CvRDT r1 s1 m, CvRDT r2 s2 m)
          => (s2 -> m s2) 
          -> CvRepCmd (r1,r2) (s1,s2) k m s2
emitSndOn f = emitSnd =<< lift . f =<< snd <$> check

-- | A "convergent sequence" in which merging preserves the relative
-- order of all elements (and does not duplicate shared sub-chains).
class (CvRDT r (c l) m) => CvChain r c l m where
  -- | Remove the last link from the chain, or return 'Nothing' if the
  -- chain is empty.
  pop :: r -> c l -> m (Maybe (l,c l))
  append :: r -> l -> c l -> m (c l)

-- | Perform a left-fold over a 'CvChain', using the provided
-- "shortcut action" to avoid loading the entire chain if possible.
-- 
-- The shortcut action is applied to each chain prefix as links are
-- popped off the end; if the action ever produces a 'Just' value,
-- that value will be used in place of the provided initial 'a' value,
-- and the fold will skip the links remaining in matched prefix.
foldlC :: (CvChain r c l m) 
       => r -- ^ Resolver
       -> (a -> l -> m a) -- ^ Left-fold update action
       -> (c l -> m (Maybe a)) -- ^ Shortcut action
       -> a -- ^ Initial value
       -> c l -- ^ Chain to fold over
       -> m a
foldlC r f short a0 c0 = 
  let load c ls = pop r c >>= \case
        Just (l,c') -> short c' >>= \case
          Just a -> foldlM f a (l:ls)
          Nothing -> load c' (l:ls)
        Nothing -> foldlM f a0 ls
  in load c0 []
