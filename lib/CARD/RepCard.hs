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

import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Conc (forkIO,registerDelay,readTVar,TVar,threadDelay)
import Control.Concurrent.STM hiding (check)
import qualified Control.Concurrent.STM as STM
import System.Random
import System.IO (hFlush,stdout)

import CARD.CvRDT
import CARD.Network
import CARD.EventGraph
import CARD.Store
import CARD.Locks
import CARD.RepCore
import CARD.LQ.Internal

type CardState i r s = (Locks i s, Hist i r s)

data Job s = Emit (Effect s) | Request (Conref s) | Release

data Manager i r s = Manager
  { manInbox :: TQueue (Either (BMsg (Locks i s, Hist i r s)) (Job s))
  , manId :: i
  , manOthers :: [i]
  , manResult :: TMVar Bool
  , manJob :: Maybe (Job s)
  , manEmitTimeout :: Maybe (TVar Bool)
  , manRand :: StdGen }

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
    _ -> do liftIO $ putStrLn "Summary miss." >> hFlush stdout
            liftIO $ putStrLn ("Summ-size now " ++ show (Map.size summs + 1))
            return (Map.insert hist s summs)

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
  rand <- getStdGen
  let manager = Manager jobQueue i os resultBox Nothing Nothing rand
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
  timeoutVar <- manEmitTimeout <$> get

  let succeed = do lstm $ putTMVar outbox True
                   modify (\m -> m {manJob = Nothing})
      failJob = do lstm $ putTMVar outbox False
                   modify (\m -> m {manJob = Nothing})

      timeout :: STM (Maybe a)
      timeout = case timeoutVar of
        Just tv -> const Nothing <$> (STM.check <=< readTVar) tv
        Nothing -> const Nothing <$> STM.check False
      
      setTimeout :: (MonadIO (Res t)) => ManM i r s k t ()
      setTimeout = return ()
      -- setTimeout = do rand <- manRand <$> get
      --                 let (time,rand') = randomR (200000,400000) rand -- 0.2s to 0.4s
      --                 modify (\m -> m {manRand = rand'})
      --                 tv <- liftIO $ registerDelay time
      --                 modify (\m -> m {manEmitTimeout = Just tv})

      cancelTimeout :: (MonadIO (Res t)) => ManM i r s k t ()
      cancelTimeout = modify (\m -> m {manEmitTimeout = Nothing})

  -- Sort newest message
  lstm (Just <$> readTQueue inbox <|> timeout) >>= \case
    Just (Right j) -> do modify (\m -> m {manJob = Just j})
                         case j of
                           Emit _ -> setTimeout
                           _ -> return ()
    Just (Left (BCast s)) -> lift (incorp s) >> return ()
    Nothing -> failJob -- Emit timeout has occured

  -- Try to finish job
  (manJob <$> get) >>= \case
    Just (Emit e) -> do
      locks <- fst <$> lift check
      if permitted i e locks
         then do modify (\m -> m {manEmitTimeout = Nothing})
                 lift (emitSndOn (append r2 (i,e)))
                 succeed
         else failJob
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
      -> Int -- ^ Delay multiplier
      -> Op s a -- ^ The operation to execute
      -> IO (Either EvalFail a,Int)
runOp jq rv sv n t = runOpR jq rv sv 0 t

-- execLQ :: (Store s) 
--        => IO s -- ^ Lookup latest value
--        -> ((Job s, LQ s a) -> IO ()) -- ^ Send to queue
--        -> LQ s a 
--        -> IO ()
-- execLQ latest enq = runLQ (\c -> enq (Request c, rc))
--   where satQuery c | c == crT = latest
--                    | True = 
--         rc = execLQ enq

runOpR :: (Store s)
       => TQueue (Either l (Job s)) -- ^ The manager's job queue
       -> TMVar Bool -- ^ The manager's result box
       -> TVar s -- ^ The latest state var
       -> Int -- ^ Delay multiplier
       -> Op s a -- ^ The operation to execute
       -> IO (Either EvalFail a,Int)
runOpR jq rv sv n t = 
  let ctx = Eval
        ef0
        (lstm . writeTQueue jq . Right)
        (lstm $ takeTMVar rv)
        (lstm $ readTVar sv)
  in runStateT (runExceptT (evalOp t)) ctx >>= \case
       (Left EvalFail,_) -> return (Left EvalFail,n)
       (Right a,ctx) -> do 
         result <- if evalEffect ctx /= ef0
                      then do (evalJobQueue ctx) (Emit (evalEffect ctx))
                              (evalJobResult ctx) >>= \case
                                True -> return (Right a)
                                False -> return (Left EvalFail)
                      else return (Right a)
         (evalJobQueue ctx) Release
         (evalJobResult ctx)
         case result of
           Left _ -> do putStrLn $ "Failed, retrying x" ++ show n
                        (delay,_) <- randomR (200000,400000) <$> getStdGen
                        threadDelay (delay * (2 ^ n))
                        runOpR jq rv sv (n + 1) t
           r -> return (r,n)

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
