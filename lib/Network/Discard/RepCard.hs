{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Network.Discard.RepCard 
  ( Job (..)
  , runCarolM
  , runCarolR
  , ManC
  , ManagerConn
  , initManager
  , giveUpdate
  , giveJob
  , getLatest
  , killManager

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
import System.IO (hFlush,stdout)
import Data.Time.Clock
import Data.Foldable (fold)
import Control.Lens

import Data.CvRDT
import Data.CARD.Store
import Lang.Carol
import Network.Discard.RateControl
import Network.Discard.Broadcast

data Job s m a = 
    Emit    (Effect s) (     m (Job s m a))
  | Request (Conref s) (s -> m (Job s m a))
  | Finish             (     m a          )

handleQ :: (Monad m, CARD s)
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

handleQM :: (CARD s)
         => (a -> IO ()) -- ^ Action to take on completion
         -> ManagerConn i r s -- ^ Manager to handle if necessary
         -> HelpMe (Conref s) s (a, Effect s)
         -> IO ()
handleQM fin man h = handleQ fin (getLatest man) h >>= \case
  Finish m -> m
  j -> giveJob j man

handleQR :: (CARD s)
         => ManagerConn i r s
         -> HelpMe (Conref s) s (a, Effect s)
         -> IO a
handleQR man h = do
  tmv <- newEmptyTMVarIO
  let fin a = lstm (putTMVar tmv a) >> return ()
  handleQM fin man h
  lstm $ takeTMVar tmv

-- | Run an operation, handing it off the store manager if it is not
-- trivial, and perform some final action when it is complete.  This
-- action may be performed on either the calling thread or the manager
-- thread.
runCarolM :: (CARD s) 
       => ManagerConn i r s 
       -> (a -> IO ()) -- ^ Final action to execute on completion
       -> Carol s a -- ^ Operation to run, feeding the final action
       -> IO ()
runCarolM man fin t = handleQM fin man (runCarol' t)

-- | Run an operation, handing it off the store manager if it is not
-- trivial, and return the result to the original caller
runCarolR :: (CARD s)
       => ManagerConn i r s
       -> Carol s a
       -> IO a
runCarolR man t = handleQR man (runCarol' t)

data Work j = Work j j

-- In Working, the first j is the complete, unevaluated job which will
-- be reinstated upon a retry
data Workspace j = Working j j | Idle

initJob j Idle = Working j j

stepJob j (Working j0 _) = Working j0 j

failJob :: Workspace j -> Workspace j
failJob (Working j0 _) = Idle

finishJob (Working _ _) = Idle

data Manager c i s = Manager
  { manInbox :: TQueue (Either (BMsg (Store c i s)) (Job s IO ()))
  , manId :: i
  , manOthers :: [i]
  , manJobQueue :: TQueue (Job s IO ())
  , manWaitingRoom :: [(Conref s, s -> IO (Job s IO ()))]
  , manCurrentJob :: Workspace (Job s IO ())
  , manLatest :: TVar s
  , manLatestHist :: TVar (Hist c i s)
  , manRCIndex :: RCIndex i s (Job s IO ())
  , grantMultiplex :: Int
  , batcheff :: [Effect s]
  , batchSize :: Int }

type ManM r c i s k = StateT (Manager c i s) (CvRepCmd r (Store c i s) k IO)

class (CvRDT r (Hist c i s) IO, Ord i, CARD s, CvChain r c (i, Effect s) IO) => ManC r c i s k
instance (CvRDT r (Hist c i s) IO, Ord i, CARD s, CvChain r c (i, Effect s) IO) => ManC r c i s k

data ManagerConn c i s = ManagerConn 
  { eventQueue :: TQueue (Either (BMsg (Store c i s)) (Job s IO ()))
  , latestStoreVal :: TVar s
  , latestHistVal :: TVar (Hist c i s)
  , manLoopThreadId :: ThreadId }

giveUpdate :: BMsg (Store c i s) -> ManagerConn c i s -> IO ()
giveUpdate m (ManagerConn q _ _ _) = lstm $ writeTQueue q (Left m)

giveJob :: Job s IO () -> ManagerConn c i s -> IO ()
giveJob j (ManagerConn q _ _ _) = lstm $ writeTQueue q (Right j)

getLatest :: ManagerConn c i s -> IO s
getLatest (ManagerConn _ v _ _) = readTVarIO v

getLatestHist :: ManagerConn c i s -> IO (Hist c i s)
getLatestHist (ManagerConn _ _ h _) = readTVarIO h

killManager :: ManagerConn c i s -> IO (s, Hist c i s)
killManager conn@(ManagerConn _ _ _ i) = do 
  sFinal <- getLatest conn
  hFinal <- getLatestHist conn
  killThread i
  return (sFinal, hFinal)

-- | Microseconds to seconds
ms2s :: (Fractional a) => Int -> a
ms2s ms = fromIntegral ms / 1000000

-- | Initialize a replica manager.  This returns a 'TQueue' for
-- updates from other replicas and jobs from other threads, and a
-- 'TVar' which can be read to get the latest calculated store value.
initManager :: (Ord s, ManC r c i s (), Transport t, Carries t (Store c i s), Res t ~ IO)
            => i -- ^ This replica's ID
            -> [i] -- ^ Other replicas' IDs
            -> [Dest t] -- ^ Broadcast targets
            -> r -- ^ Event graph resolver 
            -> s -- ^ Initial store value
            -> Hist c i s -- ^ Initial history
            -> s -- ^ Base store value
            -> Int -- ^ Timeout unit size (microseconds)
            -> Int -- ^ Batch size
            -> IO (ManagerConn c i s)
initManager i os ds r s0 hist0 s00 ts bsize = do
  eventQueue <- newTQueueIO
  jobQueue <- newTQueueIO
  latestState <- newTVarIO s0
  latestHist <- newTVarIO hist0
  rci <- newRCIndex (ms2s ts)
  let bc = broadcast ds
      manager = Manager
        eventQueue
        i
        os
        jobQueue
        []
        Idle
        latestState
        latestHist
        rci
        0
        []
        bsize
      onUp = upWithSumms latestState latestHist r s00
  ti <- forkIO $ do
          runCvRep
            r
            (mempty,hist0)
            (Map.fromList [(hist0,s0)])
            bc
            onUp 
            (runStateT ((lift . lift $ helloAll ds) >> managerLoop) manager)
          return ()
  return $ ManagerConn eventQueue latestState latestHist ti

managerLoop :: (ManC r c i s k) => ManM r c i s k ()
managerLoop = do
  -- Handle latest event (incorp update or enque job)
  -- liftIO $ putStrLn "Handle latest..."
  handleLatest
  -- Put jobs ready for retry back in the job queue
  -- liftIO $ putStrLn "Resurrect fails..."
  resurrectFails
  -- Work on current job or start next one, taking first from the
  -- waiting area.  New jobs that need requests have the request made
  -- and go to the waiting area.
  -- liftIO $ putStrLn "Work..."
  workOnJob
  -- Enque locking requests from other replicas
  -- liftIO $ putStrLn "Handle lock reqs..."
  handleLockReqs
  -- And grant them at the appropriate rate
  -- liftIO $ putStrLn "Grant lock reqs..."
  grantLockReqs
  -- And loop
  managerLoop

onCurrent :: (ManC r c i s k) 
          => (Workspace (Job s IO ()) -> Workspace (Job s IO ())) 
          -> ManM r c i s k ()
onCurrent f = modify (\m -> m { manCurrentJob = f (manCurrentJob m) })

onRCI :: (ManC r c i s k) 
      => (RCIndex i s (Job s IO ()) -> IO (RCIndex i s (Job s IO ())))
      -> ManM r c i s k ()
onRCI f = do rci <- manRCIndex <$> get
             rci' <- liftIO (f rci)
             modify (\m -> m {manRCIndex = rci' })

data ManEvent c i s = ManNew (Either (BMsg (Store c i s)) 
                                     (Job s IO ())) 
                    | ManRate (RCIndex i s (Job s IO ())) 

handleLatest :: (ManC r c i s k) => ManM r c i s k ()
handleLatest = do
  man <- get
  let updates = (ManRate <$> awaitTimeouts (manRCIndex man))
                <|> (ManNew <$> readTQueue (manInbox man)) 
  lstm updates >>= \case
    ManNew (Right j) -> case j of
      _ -> lstm $ writeTQueue (manJobQueue man) j
    ManNew (Left (BCast s)) -> 
      lift (incorp s) >> return ()
    ManNew (Left Hello) ->
      -- Respond to a 'Hello' by broadcasting the current state,
      -- bringing the new node up to date.
      lift bcast >> return ()
    ManRate rci' -> do
      -- liftIO $ putStrLn "RateControl event."
      modify (\m -> m { manRCIndex = rci' })


getWaiting :: (ManC r c i s k) => ManM r c i s k (Maybe (Job s IO ()))
getWaiting = do
  i <- manId <$> get
  others <- manOthers <$> get
  ls <- lift.use $ store.locks
  wr <- manWaitingRoom <$> get
  let findReady (c,fj) (Nothing,wr') = 
        if (holding' i ls `impl` c) && confirmed i others ls
           then (Just fj,wr')
           else (Nothing, (c,fj):wr')
      findReady cfj (fj,wr') = (fj,cfj:wr')
      (r,wr') = foldr findReady (Nothing,[]) wr
  case r of
    Just fj -> do
      modify (\m -> m { manWaitingRoom = wr' })
      modify (\m -> m { grantMultiplex = grantMultiplex m + 1 })
      Just <$> (liftIO . fj =<< manGetLatest)
    Nothing -> return Nothing

anyWaiting :: (ManC r c i s k) => ManM r c i s k Bool
anyWaiting = manWaitingRoom <$> get >>= \case
    [] -> return False
    _ -> return True

manGetLatest :: (ManC r c i s k) => ManM r c i s k s
manGetLatest = lstm . readTVar =<< manLatest <$> get

putOnHold :: (ManC r c i s k) => Conref s -> (s -> IO (Job s IO ())) -> ManM r c i s k ()
putOnHold c fj = do
  i <- manId <$> get
  lift (emitOn' locks $ return . request i c)
  modify (\m -> m { manWaitingRoom = (c,fj) : manWaitingRoom m })

histAppend' :: (ManC r c i s k) => Effect s -> ManM r c i s k ()
histAppend' e = do
  i <- manId <$> get
  lift $ histAppend i e

sendBatch :: (ManC r c i s k) => ManM r c i s k ()
sendBatch = do
  beff <- batcheff <$> get
  if length beff > 0
     then do histAppend' (fold beff)
             modify $ \m -> m { batcheff = [] }
     else return ()

enbatch :: (ManC r c i s k) => Effect s -> ManM r c i s k ()
enbatch e = do
  bsize <- batchSize <$> get
  beff' <- (e:) . batcheff <$> get
  modify $ \m -> m { batcheff = beff' }
  if length beff' >= bsize
     then sendBatch
     else return ()

workOnJob :: (ManC r c i s k) => ManM r c i s k ()
workOnJob = manCurrentJob <$> get >>= \case

  Working j0 j -> do
    i <- manId <$> get
    others <- manOthers <$> get
    ls <- lift.use $ store.locks
    case j of -- do work!
      Request c f -> do
        liftIO $ putStrLn "Handling nested request..."
        if not $ requested i c ls
           then lift (emitOn' locks $ return . request i c) >> return ()
           else if confirmed i others ls
                   then do advJob . f =<< lstm . readTVar =<< manLatest <$> get
                           workOnJob
                   else return ()
      Emit e m -> do
        case permitted' i e ls of
          Right () -> do enbatch e
                         onRCI $ reportSuccess e
                         -- liftIO $ putStrLn "Emit!"
                         rci <- manRCIndex <$> get
                         if checkBlock (getRCBlocker rci) e
                            then resurrectFails
                            else return ()
                         advJob m
                         workOnJob
          Left c -> do onCurrent failJob
                       onRCI $ reportFailure c j0
      Finish m -> do 
        -- Perform finishing callback
        liftIO m
        onCurrent finishJob
        workOnJob

  Idle -> do -- No current job, try to pop from queue
    i <- manId <$> get
    ls <- lift.use $ store.locks
    let releaseAll = 
          if holding i ls
             then do liftIO (putStrLn "Releasing locks...")
                     mx <- grantMultiplex <$> get 
                     if mx > 1
                        then liftIO (putStrLn $ "Grant multiplex: " ++ show mx)
                        else return ()
                     modify (\m -> m { grantMultiplex = 0 })
                     lift (emitOn' locks $ return . release i) >> return ()
             else return ()
    -- Try waiting room first
    getWaiting >>= \case
      Just j -> onCurrent (initJob j) >> workOnJob
      Nothing -> do
        -- If no one is ready in waiting room, take from main queue
        aw <- anyWaiting
        -- If no one is in the waiting room at all, release all locks
        if not aw
           then releaseAll
           else return ()
        manJobQueue <$> get >>= lstm . tryReadTQueue >>= \case
          -- Send initial requests to waiting room
          Just (Request c fj) -> putOnHold c fj >> workOnJob
          -- Anything else is good to go
          Just j -> onCurrent (initJob j) >> workOnJob
          -- If queue is empty, there is nothing left to do for now
          Nothing -> return ()

advJob :: (ManC r c i s k) => IO (Job s IO ()) -> ManM r c i s k ()
advJob = (onCurrent . stepJob =<<) . liftIO

handleLockReqs :: (ManC r c i s k) => ManM r c i s k ()
handleLockReqs = do
  i <- manId <$> get
  rci <- manRCIndex <$> get
  ls <- lift.use $ store.locks
  rci' <- liftIO $ foldM (flip enqueGrant) rci (ungranted i ls)
  
  modify (\m -> m { manRCIndex = rci' })

grantLockReqs :: (ManC r c i s k) => ManM r c i s k ()
grantLockReqs = do
  man <- get
  liftIO (getGrant (manRCIndex man)) >>= \case
    Just ((i2,c),rci') -> do 
      sendBatch
      modify $ \m -> m { manRCIndex = rci' }
      lift . emitOn' locks $ \ls -> return (grant (manId man) i2 ls)
      liftIO $ putStrLn "Granted lock."
      return ()
    Nothing -> return ()

resurrectFails :: (ManC r c i s k) => ManM r c i s k ()
resurrectFails = do
  man <- get
  liftIO (getRetry (manRCIndex man)) >>= \case
    Just (j,rci') -> 
      do case j of
           Request c fj -> putOnHold c fj
           _ -> lstm $ writeTQueue (manJobQueue man) j
         modify (\m -> m { manRCIndex = rci' })
         resurrectFails
    Nothing -> return ()

lstm :: MonadIO m => STM a -> m a
lstm = liftIO . atomically

upWithSumms :: (MonadIO m, Ord s, CARD s, Ord (Hist c i s), CvChain r c (i, Effect s) m)
            => TVar s -- ^ Location to post result
            -> TVar (Hist c i s) -- ^ Location to post history
            -> r -- ^ Resolver
            -> s -- ^ Initial value
            -> (s1, Hist c i s) -- ^ History to evaluate
            -> Map (Hist c i s) s -- ^ Summaries to use
            -> m (Map (Hist c i s) s)
upWithSumms post postHist r s0 (_,hist) summs = do
  s <- evalHist r s0 hist summs
  lstm $ swapTVar post s
  lstm $ swapTVar postHist hist
  return (Map.insert hist s summs)
