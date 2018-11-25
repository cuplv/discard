{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.CvRDT
  ( CvRDT (..)
  , CvReplica
  , CvRepCmd
  , runCvReplica
  , bcast
  , check
  , checkMeta
  , resolver
  , incorp
  , emit
  , emitFst
  , emitSnd
  , emitOn
  , emitFstOn
  , emitSndOn
  ) where

import Control.Monad.State

class (Eq s, Monad m) => CvRDT r s m where
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

runCvReplica :: (CvRDT r s m)
             => r -- ^ Resolver
             -> k -- ^ Initial meta-state
             -> (s -> m ()) -- ^ Broadcast action
             -> (s -> k -> m k) -- ^ On-update action
             -> StateT (CvReplica r s k m) m a -- ^ Command
             -> m a
runCvReplica r k bc ou cmd = do 
  s0 <- cvempty r
  fst <$> runStateT cmd (CvReplica s0 k r ou bc)

bcast :: (CvRDT r s m) => StateT (CvReplica r s k m) m ()
bcast = do s <- cvrState <$> get
           bf <- cvrBroadcast <$> get
           lift $ bf s

check :: (Monad m) => StateT (CvReplica r s k m) m s
check = cvrState <$> get

resolver :: (CvRDT r s m) => StateT (CvReplica r s k m) m r
resolver = cvrResolver <$> get

checkMeta :: (CvRDT r s m) => StateT (CvReplica r s k m) m k
checkMeta = cvrMetaState <$> get

updateMeta :: (CvRDT r s m) => StateT (CvReplica r s k m) m ()
updateMeta = do 
  rep <- get
  k' <- lift$ (cvrOnUpdate rep) (cvrState rep) (cvrMetaState rep)
  put $ rep { cvrMetaState = k' }

incorp :: (CvRDT r s m) => s -> StateT (CvReplica r s k m) m (s,Bool)
incorp s2 = do 
  rep <- get
  s3 <- lift$ cvmerge (cvrResolver rep) (cvrState rep) s2
  if s3 /= cvrState rep
     then do put $ rep { cvrState = s3 }
             updateMeta
             return (s3,True)
     else return (s3,False)

emit :: (CvRDT r s m) => s -> StateT (CvReplica r s k m) m s
emit s = do (s',change) <- incorp s
            if change
               then bcast
               else return ()
            return s'

emitFst :: (CvRDT r1 s1 m, CvRDT r2 s2 m) 
        => s1 
        -> StateT (CvReplica (r1,r2) (s1,s2) k m) m s1
emitFst s1 = do 
  res <- snd . cvrResolver <$> get
  s2 <- lift$ cvempty res
  fst <$> emit (s1,s2)

emitSnd :: (CvRDT r1 s1 m, CvRDT r2 s2 m) 
        => s2 
        -> StateT (CvReplica (r1,r2) (s1,s2) k m) m s2
emitSnd s2 = do 
  res <- fst . cvrResolver <$> get
  s1 <- lift$ cvempty res
  snd <$> emit (s1,s2)

emitOn :: (CvRDT r s m) => (s -> m s) -> StateT (CvReplica r s k m) m s
emitOn f = emit =<< lift . f =<< check

emitFstOn :: (CvRDT r1 s1 m, CvRDT r2 s2 m)
          => (s1 -> m s1) 
          -> StateT (CvReplica (r1,r2) (s1,s2) k m) m s1
emitFstOn f = emitFst =<< lift . f =<< fst <$> check

emitSndOn :: (CvRDT r1 s1 m, CvRDT r2 s2 m)
          => (s2 -> m s2) 
          -> StateT (CvReplica (r1,r2) (s1,s2) k m) m s2
emitSndOn f = emitSnd =<< lift . f =<< snd <$> check
