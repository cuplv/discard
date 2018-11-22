{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Data.CvRDT.Replica
  ( Core
  , CoreM
  , CoreC
  , runCoreM
  , runCoreM'
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

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Concurrent.STM hiding (check)
import Control.Monad.State

import Data.CvRDT
import Data.CvRDT.Broadcast

data Core r s k t = Core 
  { coreState :: s
  , coreMetaState :: k
  , coreResolver :: r
  , coreOnUpdate :: s -> k -> Res t k
  , coreDests :: [Dest t] }

type CoreM r s k t = StateT (Core r s k t) (Res t)

class (Ord s, Transport t, Res t ~ IO, Carries t s, CvRDT r s IO) => CoreC r s k t

instance (Ord s, Transport t, Res t ~ IO, Carries t s, CvRDT r s IO) => CoreC r s k t

runCoreM :: (CoreC r s k t) 
         => r 
         -> k 
         -> [Dest t] 
         -> (s -> k -> Res t k) 
         -> CoreM r s k t a 
         -> Res t a
runCoreM r k ds cb f = do s0 <- cvempty r
                          fst <$> runStateT f (Core s0 k r cb ds)

runCoreM' :: (CoreC r s () t) 
          => r 
          -> [Dest t] 
          -> CoreM r s () t a 
          -> Res t a
runCoreM' r ds f = runCoreM r () ds (\_ _ -> return ()) f

bcast :: (CoreC r s k t) => CoreM r s k t ()
bcast = do s <- coreState <$> get
           others <- coreDests <$> get
           liftIO$ mapM_ (flip send (BCast s)) others

check :: (CoreC r s k t) => CoreM r s k t s
check = coreState <$> get

resolver :: (CoreC r s k t) => CoreM r s k t r
resolver = coreResolver <$> get

checkMeta :: (CoreC r s k t) => CoreM r s k t k
checkMeta = coreMetaState <$> get

updateMeta :: (CoreC r s k t) => CoreM r s k t ()
updateMeta = do 
  core <- get
  k' <- lift$ (coreOnUpdate core) (coreState core) (coreMetaState core)
  put $ core { coreMetaState = k' }

incorp :: (CoreC r s k t) => s -> CoreM r s k t (s,Bool)
incorp s2 = do 
  core <- get
  s3 <- lift$ merge (coreResolver core) (coreState core) s2
  if s3 /= coreState core
     then do put $ core { coreState = s3 }
             updateMeta
             return (s3,True)
     else return (s3,False)

emit :: (CoreC r s k t) => s -> CoreM r s k t s
emit s = do (s',change) <- incorp s
            if change
               then bcast
               else return ()
            return s'

emitFst :: (CoreC (r1,r2) (s1,s2) k t, CvRDT r2 s2 IO) 
        => s1 
        -> CoreM (r1,r2) (s1,s2) k t s1
emitFst s1 = do 
  res <- snd . coreResolver <$> get
  s2 <- lift$ cvempty res
  fst <$> emit (s1,s2)

emitSnd :: (CoreC (r1,r2) (s1,s2) k t, CvRDT r1 s1 IO) 
        => s2 
        -> CoreM (r1,r2) (s1,s2) k t s2
emitSnd s2 = do 
  res <- fst . coreResolver <$> get
  s1 <- lift$ cvempty res
  snd <$> emit (s1,s2)

emitOn :: (CoreC r s k t) => (s -> Res t s) -> CoreM r s k t s
emitOn f = emit =<< lift . f =<< check

emitFstOn :: (CoreC (r1,r2) (s1,s2) k t, CvRDT r2 s2 IO)
          => (s1 -> Res t s1) 
          -> CoreM (r1,r2) (s1,s2) k t s1
emitFstOn f = emitFst =<< lift . f =<< fst <$> check

emitSndOn :: (CoreC (r1,r2) (s1,s2) k t, CvRDT r1 s1 IO)
          => (s2 -> Res t s2) 
          -> CoreM (r1,r2) (s1,s2) k t s2
emitSndOn f = emitSnd =<< lift . f =<< snd <$> check
