{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.CvRDT
  ( CvRDT (..)
  -- * Core CvRDT replica logic
  , CvRepCmd
  , CvReplica
  , runCvRep
  , runCvRep'
  , check
  , incorp
  , emit
  , emitOn
  , bcast
  , checkMeta
  , getResolver
  -- * Lens interface
  , store
  , storeMeta
  , resolver
  , incorp'
  , incorpOn'
  , emit'
  , emitOn'
  -- * CvRDT sub-classes
  , CvChain (..)
  , foldlC
  , foldlCM
  ) where

import Control.Monad.State
import Data.Foldable (foldlM)
import Control.Lens

class (Ord s, Monad m) => CvRDT r s m where
  cvmerge :: r -> s -> s -> m s
  cvempty :: r -> m s

instance (CvRDT r s1 m, CvRDT r s2 m) => CvRDT r (s1,s2) m where
  cvmerge r (s1,s2) (s3,s4) = (,) <$> cvmerge r s1 s3 <*> cvmerge r s2 s4
  cvempty r = (,) <$> cvempty r <*> cvempty r

data CvReplica r s k m = CvReplica
  { _cvrState :: s
  , _cvrMetaState :: k
  , _cvrResolver :: r
  , _cvrOnUpdate :: s -> k -> m k
  , _cvrBroadcast :: s -> m () }

makeLenses ''CvReplica

type CvRepCmd r s k m = StateT (CvReplica r s k m) m

-- | Run a replica managing a 'CvRDT' store over a series of commands.
runCvRep :: (CvRDT r s m)
         => r -- ^ Resolver
         -> s -- ^ Initial state
         -> k -- ^ Initial meta-state
         -> (s -> m ()) -- ^ Broadcast action
         -> (s -> k -> m k) -- ^ On-update action
         -> CvRepCmd r s k m a -- ^ Command script
         -> m a
runCvRep r s0 k0 bc ou cmd = 
  fst <$> runStateT cmd (CvReplica s0 k0 r ou bc)

-- | Run a replica with an empty initial state and no meta-state or
-- update action
runCvRep' :: (CvRDT r s m) 
          => r -- ^ Resolver
          -> (s -> m ())  -- ^ Broadcast action
          -> CvRepCmd r s () m a -- ^ Command script
          -> m a
runCvRep' r bc sc = do s0 <- cvempty r
                       runCvRep r s0 () bc (\_ _ -> return ()) sc

-- | Broadcast the current state.
bcast :: (CvRDT r s m) => CvRepCmd r s k m ()
bcast = lift =<< use cvrBroadcast <*> use store

-- | Get the current state.
check :: (Monad m) => CvRepCmd r s k m s
check = use store

-- | Get the state resolver.
getResolver :: (CvRDT r s m) => CvRepCmd r s k m r
getResolver = use cvrResolver

-- | Get the current meta-state.
checkMeta :: (CvRDT r s m) => CvRepCmd r s k m k
checkMeta = use cvrMetaState

-- | Re-run the meta update action on the current state.
--
-- The meta update action is automatically run when the state is
-- changed using any of the following actions ('incorp', 'emit',
-- 'emitOn', etc.).
updateMeta :: (CvRDT r s m) => CvRepCmd r s k m ()
updateMeta = cvrMetaState <~ 
  (lift 
   =<< use cvrOnUpdate 
   <*> use store 
   <*> use cvrMetaState)

-- | Merge a state into the replica's current state.  The return value
-- is the new combined state, or 'Nothing' if the merge made no
-- change.
--
-- This action triggers the 'updateMeta' action when the state has
-- changed.
incorp :: (CvRDT r s m) => s -> CvRepCmd r s k m (Maybe s)
incorp = incorp' id

-- | 'incorp' through a lens, merging the provided value into only the
-- state component defined by the lens.
incorp' :: (CvRDT r s m, CvRDT r t m) 
        => Lens' s t
        -> t
        -> CvRepCmd r s k m (Maybe t)
incorp' ls t2 = do
  r <- use cvrResolver
  t1 <- use $ store.ls
  t3 <- lift $ cvmerge r t1 t2
  if t1 == t3
     then return Nothing
     else do cvrState.ls .= t3
             updateMeta
             return (Just t3)

incorpOn'
  :: (CvRDT r s m, CvRDT r s1 m)
  => Lens' s s1
  -> (s1 -> m s1)
  -> CvRepCmd r s k m (Maybe s1)
incorpOn' ls f = incorp' ls =<< lift . f =<< use (store.ls)

-- | Merge a new state into the replica's current state and broadcast
-- the new current state (only if it has changed).  The return value
-- is the new current state.
emit :: (CvRDT r s m) => s -> CvRepCmd r s k m s
emit = emit' id

-- | 'emit' through a lens.
emit' :: (CvRDT r s m, CvRDT r t m) 
      => Lens' s t
      -> t
      -> CvRepCmd r s k m t
emit' ls s = incorp' ls s >>= \case
  Just s' -> bcast >> return s
  Nothing -> use (store.ls)

-- | Run an action on the current state, incorporating and emitting
-- its result as the new current state.  The return value is the new
-- current state.
emitOn :: (CvRDT r s m) => (s -> m s) -> CvRepCmd r s k m s
emitOn = emitOn' id

-- | 'emitOn' through a lens.
emitOn' :: (CvRDT r s m, CvRDT r s1 m) 
        => Lens' s s1
        -> (s1 -> m s1)
        -> CvRepCmd r s k m s1
emitOn' ls f = emit' ls =<< lift . f =<< use (store.ls)

-- | A "convergent sequence" in which merging preserves the relative
-- order of all elements (and does not duplicate shared sub-chains).
class (CvRDT r (c l) m) => CvChain r c l m where
  -- | Remove the last link from the chain, or return 'Nothing' if the
  -- chain is empty.
  pop :: r -> c l -> m (Maybe (l,c l))
  append :: r -> l -> c l -> m (c l)

-- | Perform a left-fold over a 'CvChain', using the provided
-- "shortcut function" to avoid loading the entire chain if possible.
-- 
-- The shortcut function is applied to each chain prefix as links are
-- popped off the end; if the function ever produces a 'Just' value,
-- that value will be used in place of the provided initial 'a' value,
-- and the fold will skip the links remaining in matched prefix.
foldlC :: (CvChain r c l m) 
       => r -- ^ Resolver
       -> (a -> l -> a) -- ^ Left-fold update function
       -> (c l -> Maybe a) -- ^ Shortcut function
       -> a -- ^ Initial value
       -> c l -- ^ Chain to fold over
       -> m a
foldlC r f short = foldlCM r (\a b -> return $ f a b) (return . short)

-- | Just like 'foldlC', except that the update and shortcut functions
-- can be monadic actions.
foldlCM :: (CvChain r c l m) 
        => r -- ^ Resolver
        -> (a -> l -> m a) -- ^ Left-fold update action
        -> (c l -> m (Maybe a)) -- ^ Shortcut action
        -> a -- ^ Initial value
        -> c l -- ^ Chain to fold over
        -> m a
foldlCM r f short a0 c0 = 
  let load c ls = pop r c >>= \case
        Just (l,c') -> short c' >>= \case
          Just a -> foldlM f a (l:ls)
          Nothing -> load c' (l:ls)
        Nothing -> foldlM f a0 ls
  in load c0 []

-- | Current store value
store :: Getter (CvReplica r s k m) s
store = to $ view cvrState

-- | Current store meta-state value
storeMeta :: Getter (CvReplica r s k m) k
storeMeta = to $ view cvrMetaState

-- | Store resolver
resolver :: Getter (CvReplica r s k m) r
resolver = to $ view cvrResolver
