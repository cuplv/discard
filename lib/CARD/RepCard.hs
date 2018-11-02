{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module CARD.RepCard 
  ( Job
  , CardState
  , ManC
  , initManager
  , runOp
  , EvalFail

  , module CARD.RepCore

  ) where

import Control.Monad.State
import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Concurrent (forkIO)
import Control.Concurrent.STM hiding (check)

import CARD.CvRDT
import CARD.Network
import CARD.EventGraph
import CARD.Store
import CARD.Locks
import CARD.RepCore
import CARD.LQ

type CardState i r s = (Locks i s, Hist i r s)

data Job s = Emit (Effect s) | Request (Conref s) | Release

data Manager i r s = Manager
  { manInbox :: TQueue (Either (BMsg (Locks i s, Hist i r s)) (Job s))
  , manId :: i
  , manOthers :: [i]
  , manResult :: TMVar Bool
  , manJob :: Maybe (Job s) }

type ManM i r s k t = StateT (Manager i r s) (CoreM ((),r) (Locks i s, Hist i r s) k t)

class (CoreC ((),r) (Locks i s, Hist i r s) k t, Ord i, Store s, EG r (i, Effect s) (Res t)) => ManC i r s k t

instance (CoreC ((),r) (Locks i s, Hist i r s) k t, Ord i, Store s, EG r (i, Effect s) (Res t)) => ManC i r s k t

lstm :: MonadIO m => STM a -> m a
lstm = liftIO . atomically

upWithSumms :: (EGS i r s m, MonadIO m, Ord s) 
            => TVar s -- ^ Location to post result
            -> (r1,r) -- ^ Resolver
            -> s -- ^ Initial value
            -> (s1, Hist i r s) -- ^ History to evaluate
            -> Summaries i r s -- ^ Summaries to use
            -> m (Summaries i r s)
upWithSumms post (_,r) s0 (_,hist) summs = do
  (s,result) <- evalHistS r s0 summs hist
  lstm $ swapTVar post s
  case result of
    Hit -> return summs
    _ -> return (Map.insert hist s summs)

initManager :: (Ord s, ManC i r s () t) 
            => i 
            -> [i]
            -> [Dest t]
            -> r
            -> s -- ^ Initial store value
            -> IO ( TQueue (Either (BMsg (CardState i r s)) (Job s))
                  , TMVar Bool
                  , TVar s )
initManager i os ds r s0 = do
  jobQueue <- lstm newTQueue
  resultBox <- lstm newEmptyTMVar
  latestState <- lstm $ newTVar s0
  let manager = Manager jobQueue i os resultBox Nothing
      onUp = upWithSumms latestState ((),r) s0

  forkIO $ do
    runCoreM ((),r) Map.empty ds onUp (runStateT managerLoop manager)
    return ()

  return (jobQueue, resultBox, latestState)

managerLoop :: (ManC i r s k t) => ManM i r s k t ()
managerLoop = do
  i <- manId <$> get
  (r1,r2) <- lift resolver
  others <- manOthers <$> get
  inbox <- manInbox <$> get
  outbox <- manResult <$> get
  
  let succeed = do lstm $ putTMVar outbox True
                   modify (\m -> m {manJob = Nothing})

  -- Sort newest message
  lstm (readTQueue inbox) >>= \case
    Right j -> modify (\m -> m {manJob = Just j})
    Left (BCast s) -> lift (incorp s) >> return ()

  -- Try to finish job
  (manJob <$> get) >>= \case
    Just (Emit e) -> do
      locks <- fst <$> lift check
      if permitted i e locks
         then lift (emitSndOn (append r2 (i,e))) >> succeed
         else return ()
    Just (Request c) -> do 
      locks <- fst <$> lift check
      if not $ requested i c locks
         then lift (emitFstOn $ return . request i c) >> return ()
         else if confirmed i others locks
                 then succeed
                 else return ()
    Just Release -> do
      locks <- fst <$> lift check
      if holding i locks
         then lift (emitFstOn $ return . release i) >> return ()
         else return ()
      succeed
    Nothing -> return ()

  -- Handle open locking requests
  lift . emitFstOn $ \ls -> return $ foldr (grant i) ls others
  
  -- Loop
  managerLoop

-- | Run an operation, returning either its result or a failure
runOp :: (Store s)
      => TQueue (Either l (Job s)) -- ^ The manager's job queue
      -> TMVar Bool -- ^ The manager's result box
      -> TVar s -- ^ The latest state var
      -> Op s a -- ^ The operation to execute
      -> IO (Either EvalFail a)
runOp jq rv sv t = 
  let ctx = Eval
        ef0
        (lstm . writeTQueue jq . Right)
        (lstm $ takeTMVar rv)
        (lstm $ readTVar sv)
  in runStateT (runExceptT (evalOp t)) ctx >>= \case
       (Left EvalFail,_) -> return (Left EvalFail)
       (Right a,ctx) -> do 
         result <- if evalEffect ctx /= ef0
                      then do (evalJobQueue ctx) (Emit (evalEffect ctx))
                              (evalJobResult ctx) >>= \case
                                True -> return (Right a)
                                False -> return (Left EvalFail)
                      else return (Right a)
         (evalJobQueue ctx) Release
         (evalJobResult ctx)
         return result

data Eval s = Eval
  { evalEffect :: Effect s
  , evalJobQueue :: Job s -> IO ()
  , evalJobResult :: IO Bool
  , evalLatest :: IO s }

data EvalFail = EvalFail deriving (Show,Eq,Ord)

type EvalM s m = ExceptT EvalFail (StateT (Eval s) m)

evalOp :: (MonadIO m) => Op s a -> EvalM s m a
evalOp t = do 
  ctx <- lift get
  case t of
    Pure a -> return a
    Free (Issue e t') -> do
      lift $ modify (\m -> m {evalEffect = evalEffect m |>| e})
      evalOp t'
    Free (Query c ft') -> do
      liftIO $ (evalJobQueue ctx) (Request c)
      liftIO (evalJobResult ctx) >>= \case
        True -> do evalOp . ft' =<< liftIO (evalLatest ctx)
        False -> throwError EvalFail
