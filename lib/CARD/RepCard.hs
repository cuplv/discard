{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module CARD.RepCard 
  ( Job (..)
  , runLQM
  , runLQR
  , CardState
  , ManC
  , ManagerConn
  , initManager
  , giveUpdate
  , giveJob
  , getLatest
  , killManager

  , module CARD.RepCore

  ) where

import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Conc (forkIO,registerDelay,readTVar,TVar,threadDelay,ThreadId,killThread)
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
import CARD.LQ

type CardState i r s = (Locks i s, Hist i r s)

data Job s m a = 
    Emit    (Effect s) (     m (Job s m a))
  | Request (Conref s) (s -> m (Job s m a))
  | Finish             (     m a          )

handleQ :: (Monad m, Store s)
        => (r -> m a) -- ^ Return callback
        -> m s -- ^ Latest store ref
        -> HelpMe (Conref s) s (r, Effect s)
        -> m (Job s m a)
handleQ fin latest = \case
  HelpMe c f 
    | c == crT -> handleQ fin latest . f =<< latest
    | otherwise -> return (Request c (handleQ fin latest . f))
  GotIt (r,e) 
    | e == ef0 -> return finish
    | otherwise -> return (Emit e (return finish))
    where finish = Finish (fin r)

-- | Run an operation, handing it off the store manager if it is not
-- trivial, and perform some final action when it is complete.  This
-- action may be performed on either the calling thread or the manager
-- thread.
handleQM :: (Store s)
         => (a -> IO ()) -- ^ Action to take on completion
         -> ManagerConn i r s -- ^ Manager to handle if necessary
         -> HelpMe (Conref s) s (a, Effect s)
         -> IO ()
handleQM fin man h = handleQ fin (getLatest man) h >>= \case
  Finish m -> m
  j -> giveJob j man

-- | Run an operation, handing it off the store manager if it is not
-- trivial, and return the result to the original caller
handleQR :: (Store s)
         => ManagerConn i r s
         -> HelpMe (Conref s) s (a, Effect s)
         -> IO a
handleQR man h = do
  tmv <- newEmptyTMVarIO
  let fin a = lstm (putTMVar tmv a) >> return ()
  handleQM fin man h
  lstm $ takeTMVar tmv

runLQM :: (Store s) 
       => ManagerConn i r s 
       -> (a -> IO ()) 
       -> LQ s a 
       -> IO ()
runLQM man fin t = handleQM fin man (runLQ' t)

runLQR :: (Store s)
       => ManagerConn i r s
       -> LQ s a
       -> IO a
runLQR man t = handleQR man (runLQ' t)

-- In Working, the first j is the complete, unevaluated job which will
-- be reinstated upon a retry
data Workspace j = Working j j | Waiting j | Idle

initJob j Idle = Working j j
initJob _ w = w

stepJob j (Working j0 _) = Working j0 j
stepJob _ w = w

failJob (Working j0 _) = Waiting j0
failJob w = w

restartJob (Waiting j) = Working j j
restartJob w = w

finishJob (Working _ _) = Idle
finishJob w = w

data Manager i r s = Manager
  { manInbox :: TQueue (Either (BMsg (CardState i r s)) (Job s IO ()))
  , manId :: i
  , manOthers :: [i]
  , manJobQueue :: TQueue (Job s IO ())
  , manCurrentJob :: Workspace (Job s IO ())
  , manRand :: StdGen
  , manTimeoutVar :: Maybe (TVar Bool)
  , manTimeoutSize :: Int
  , manTimeoutBase :: Int
  , manLatest :: TVar s }

type ManM i r s k t = StateT (Manager i r s) (CoreM ((),r) (CardState i r s) k t)

class (CoreC ((),r) (CardState i r s) k t, Ord i, Store s, EG r (i, Effect s) (Res t)) => ManC i r s k t
instance (CoreC ((),r) (CardState i r s) k t, Ord i, Store s, EG r (i, Effect s) (Res t)) => ManC i r s k t

data ManagerConn i r s = ManagerConn 
  { eventQueue :: TQueue (Either (BMsg (CardState i r s)) (Job s IO ()))
  , latestStoreVal :: TVar s
  , manLoopThreadId :: ThreadId}

giveUpdate :: BMsg (CardState i r s) -> ManagerConn i r s -> IO ()
giveUpdate m (ManagerConn q _ _) = lstm $ writeTQueue q (Left m)

giveJob :: Job s IO () -> ManagerConn i r s -> IO ()
giveJob j (ManagerConn q _ _) = lstm $ writeTQueue q (Right j)

getLatest :: ManagerConn i r s -> IO s
getLatest (ManagerConn _ v _) = readTVarIO v

killManager :: ManagerConn i r s -> IO ()
killManager (ManagerConn _ _ i) = killThread i

-- | Initialize a replica manager.  This returns a 'TQueue' for
-- updates from other replicas and jobs from other threads, and a
-- 'TVar' which can be read to get the latest calculated store value.
initManager :: (Ord s, ManC i r s () t)
            => i -- ^ This replica's ID
            -> [i] -- ^ Other replicas' IDs
            -> [Dest t] -- ^ Broadcast targets
            -> r -- ^ Event graph resolver 
            -> s -- ^ Initial store value
            -> Int -- ^ Timeout unit size (microseconds)
            -> IO (ManagerConn i r s)
initManager i os ds r s0 ts = do
  eventQueue <- newTQueueIO
  jobQueue <- newTQueueIO
  latestState <- newTVarIO s0
  rand <- getStdGen
  let manager = Manager
        eventQueue
        i
        os
        jobQueue
        Idle
        rand
        Nothing
        1
        ts
        latestState
      onUp = upWithSumms latestState ((),r) s0
  ti <- forkIO $ do
          runCoreM ((),r) Map.empty ds onUp (runStateT managerLoop manager)
          return ()
  return $ ManagerConn eventQueue latestState ti

managerLoop :: (ManC i r s k t) => ManM i r s k t ()
managerLoop = do
  -- Handle latest event
  handleLatest
  -- Work on current job or start next one
  workOnJob
  -- Grant locking requests from other replicas
  handleLockReqs
  -- And loop
  managerLoop

onCurrent :: (ManC i r s k t) 
          => (Workspace (Job s IO ()) -> Workspace (Job s IO ())) 
          -> ManM i r s k t ()
onCurrent f = modify (\m -> m { manCurrentJob = f (manCurrentJob m) })

handleLatest :: (ManC i r s k t) => ManM i r s k t ()
handleLatest = do
  man <- get
  let timeout :: STM (Maybe a)
      timeout = case manTimeoutVar man of
        Just tv -> const Nothing <$> (STM.check <=< readTVar) tv
        Nothing -> const Nothing <$> STM.check False
  lstm (Just <$> readTQueue (manInbox man) <|> timeout) >>= \case
    Just (Right j) -> -- enque job
      lstm $ writeTQueue (manJobQueue man) j 
    Just (Left (BCast s)) -> -- incorporate update
      lift (incorp s) >> return ()
    Nothing -> -- retry time is up, restart job
      onCurrent restartJob

workOnJob :: (ManC i r s k t) => ManM i r s k t ()
workOnJob = manCurrentJob <$> get >>= \case

  Working _ j -> do
    i <- manId <$> get
    others <- manOthers <$> get
    locks <- fst <$> lift check
    case j of -- do work!
      Request c f -> do
        if not $ requested i c locks
           then lift (emitFstOn $ return . request i c) >> return ()
           else if confirmed i others locks
                   then do advJob . f =<< lstm . readTVar =<< manLatest <$> get
                           workOnJob
                   else return ()
      Emit e m -> do
        if permitted i e locks
           then do r2 <- snd <$> lift resolver
                   lift (emitSndOn (append r2 (i,e)))
                   modify (\m -> m {manTimeoutSize = max 1 (manTimeoutSize m - 1)})
                   advJob m
                   workOnJob
           else handleFailure
      Finish m -> do 
        -- Perform finishing callback
        liftIO m
        -- Release any held locks
        if holding i locks
           then lift (emitFstOn $ return . release i) >> return ()
           else return ()
        onCurrent finishJob
        workOnJob

  Waiting j -> -- waiting for retry, nothing to do
    return () 

  Idle -> -- No current job, try to pop from queue
    manJobQueue <$> get >>= lstm . tryReadTQueue >>= \case
      Just j -> onCurrent (initJob j) >> workOnJob
      Nothing -> return ()

handleFailure :: (ManC i r s k t) => ManM i r s k t ()
handleFailure = do
  onCurrent failJob
  setTimeout
  modify (\m -> m {manTimeoutSize = manTimeoutSize m * 2})

setTimeout :: (ManC i r s k t) => ManM i r s k t ()
setTimeout = do
  (rmult,rand') <- randomR (0.5::Double,2.0) . manRand <$> get
  modify (\m -> m {manRand = rand'})
  time <- (\b n -> floor $ fromIntegral b * fromIntegral n * rmult)
          <$> (manTimeoutBase <$> get) 
          <*> (manTimeoutSize <$> get)
  tv <- liftIO $ registerDelay time
  modify (\m -> m {manTimeoutVar = Just tv})

advJob :: (ManC i r s k t) => IO (Job s IO ()) -> ManM i r s k t ()
advJob = (onCurrent . stepJob =<<) . liftIO

handleLockReqs :: (ManC i r s k t) => ManM i r s k t ()
handleLockReqs = do
  i <- manId <$> get
  others <- manOthers <$> get
  lift . emitFstOn $ \ls -> return $ foldr (grant i) ls others
  return ()

-- managerLoop :: (ManC i r s k t) => ManM i r s k t ()
-- managerLoop = do
--   i <- manId <$> get
--   (r1,r2) <- lift resolver
--   others <- manOthers <$> get
--   inbox <- manInbox <$> get
--   outbox <- manResult <$> get
--   timeoutVar <- manEmitTimeout <$> get

--   let succeed = do lstm $ putTMVar outbox True
--                    modify (\m -> m {manJob = Nothing})
--       failJob = do lstm $ putTMVar outbox False
--                    modify (\m -> m {manJob = Nothing})

--       timeout :: STM (Maybe a)
--       timeout = case timeoutVar of
--         Just tv -> const Nothing <$> (STM.check <=< readTVar) tv
--         Nothing -> const Nothing <$> STM.check False

--       setTimeout :: (MonadIO (Res t)) => ManM i r s k t ()
--       setTimeout = return ()
--       -- setTimeout = do rand <- manRand <$> get
--       --                 let (time,rand') = randomR (200000,400000) rand -- 0.2s to 0.4s
--       --                 modify (\m -> m {manRand = rand'})
--       --                 tv <- liftIO $ registerDelay time
--       --                 modify (\m -> m {manEmitTimeout = Just tv})

--       cancelTimeout :: (MonadIO (Res t)) => ManM i r s k t ()
--       cancelTimeout = modify (\m -> m {manEmitTimeout = Nothing})

--   -- Sort newest message
--   lstm (Just <$> readTQueue inbox <|> timeout) >>= \case
--     Just (Right j) -> do modify (\m -> m {manJob = Just j})
--                          case j of
--                            Emit _ -> setTimeout
--                            _ -> return ()
--     Just (Left (BCast s)) -> lift (incorp s) >> return ()
--     Nothing -> failJob -- Emit timeout has occured

--   -- Try to finish job
--   (manJob <$> get) >>= \case
--     Just (Emit e) -> do
--       locks <- fst <$> lift check
--       if permitted i e locks
--          then do modify (\m -> m {manEmitTimeout = Nothing})
--                  lift (emitSndOn (append r2 (i,e)))
--                  succeed
--          else failJob
--     Just (Request c) -> do 
--       locks <- fst <$> lift check
--       if not $ requested i c locks
--          then lift (emitFstOn $ return . request i c) >> return ()
--          else if confirmed i others locks
--                  then succeed
--                  else return ()
--     Just Release -> do
--       locks <- fst <$> lift check
--       if holding i locks
--          then lift (emitFstOn $ return . release i) >> return ()
--          else return ()
--       succeed
--     Nothing -> return ()

--   -- Handle open locking requests
--   lift . emitFstOn $ \ls -> return $ foldr (grant i) ls others

--   -- Loop
--   managerLoop

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
    _ -> do -- liftIO $ putStrLn "Summary miss." >> hFlush stdout
            -- liftIO $ putStrLn ("Summ-size now " ++ show (Map.size summs + 1))
            return (Map.insert hist s summs)

-- initManager :: (Ord s, ManC i r s () t) 
--             => i 
--             -> [i]
--             -> [Dest t]
--             -> r
--             -> s -- ^ Initial store value
--             -> IO ( TQueue (Either (BMsg (CardState i r s)) (Job s))
--                   , TMVar Bool
--                   , TVar s )
-- initManager i os ds r s0 = do
--   jobQueue <- lstm newTQueue
--   resultBox <- lstm newEmptyTMVar
--   latestState <- lstm $ newTVar s0
--   rand <- getStdGen
--   let manager = Manager jobQueue i os resultBox Nothing Nothing rand
--       onUp = upWithSumms latestState ((),r) s0

--   forkIO $ do
--     runCoreM ((),r) Map.empty ds onUp (runStateT managerLoop manager)
--     return ()

--   return (jobQueue, resultBox, latestState)


-- -- | Run an operation, returning either its result or a failure
-- runOp :: (Store s)
--       => TQueue (Either l (Job s)) -- ^ The manager's job queue
--       -> TMVar Bool -- ^ The manager's result box
--       -> TVar s -- ^ The latest state var
--       -> Int -- ^ Delay multiplier
--       -> Op s a -- ^ The operation to execute
--       -> IO (Either EvalFail a,Int)
-- runOp jq rv sv n t = runOpR jq rv sv 0 t


-- runOpR :: (Store s)
--        => TQueue (Either l (Job s)) -- ^ The manager's job queue
--        -> TMVar Bool -- ^ The manager's result box
--        -> TVar s -- ^ The latest state var
--        -> Int -- ^ Delay multiplier
--        -> Op s a -- ^ The operation to execute
--        -> IO (Either EvalFail a,Int)
-- runOpR jq rv sv n t = 
--   let ctx = Eval
--         ef0
--         (lstm . writeTQueue jq . Right)
--         (lstm $ takeTMVar rv)
--         (lstm $ readTVar sv)
--   in runStateT (runExceptT (evalOp t)) ctx >>= \case
--        (Left EvalFail,_) -> return (Left EvalFail,n)
--        (Right a,ctx) -> do 
--          result <- if evalEffect ctx /= ef0
--                       then do (evalJobQueue ctx) (Emit (evalEffect ctx))
--                               (evalJobResult ctx) >>= \case
--                                 True -> return (Right a)
--                                 False -> return (Left EvalFail)
--                       else return (Right a)
--          (evalJobQueue ctx) Release
--          (evalJobResult ctx)
--          case result of
--            Left _ -> do putStrLn $ "Failed, retrying x" ++ show n
--                         (delay,_) <- randomR (200000,400000) <$> getStdGen
--                         threadDelay (delay * (2 ^ n))
--                         runOpR jq rv sv (n + 1) t
--            r -> return (r,n)

-- data Eval s = Eval
--   { evalEffect :: Effect s
--   , evalJobQueue :: Job s -> IO ()
--   , evalJobResult :: IO Bool
--   , evalLatest :: IO s }

-- data EvalFail = EvalFail deriving (Show,Eq,Ord)

-- type EvalM s m = ExceptT EvalFail (StateT (Eval s) m)

-- evalOp :: (MonadIO m) => Op s a -> EvalM s m a
-- evalOp t = do 
--   ctx <- lift get
--   case t of
--     Pure a -> return a
--     Free (Issue e t') -> do
--       lift $ modify (\m -> m {evalEffect = evalEffect m |>| e})
--       evalOp t'
--     Free (Query c ft') -> do
--       liftIO $ (evalJobQueue ctx) (Request c)
--       liftIO (evalJobResult ctx) >>= \case
--         True -> do evalOp . ft' =<< liftIO (evalLatest ctx)
--         False -> throwError EvalFail
