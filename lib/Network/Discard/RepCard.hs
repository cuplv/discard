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
  , DManagerSettings (..)
  , initManager
  , defaultDManagerSettings
  , defaultDManagerSettings'
  , awaitNetwork
  , giveUpdate
  , giveJob
  , getLatestState
  , getLatestVal
  , getLatestStore
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
import Lang.Carol.Internal
import Network.Discard.RateControl
import Network.Discard.Broadcast
import Network.Discard.Crypto

data Job s m a = 
    Emit    (Effect s) (     m (Job s m a))
  | Request (Conref s) (s -> m (Job s m a))
  | Finish             (     m a          )

handleQ' :: (Monad m, CARD s)
        => (r -> m a) -- ^ Return callback
        -> m s -- ^ Latest store ref
        -> HelpMe (Conref s) s (r, Effect s)
        -> m (Job s m a)
handleQ' fin latest = \case
  HelpMe c f 
    | c == crT -> handleQ' fin latest . f =<< latest
    | otherwise -> return (Request c (handleQ' fin latest . f))
  GotIt (r,e) 
    | e == ef0 -> return finish
    | otherwise -> return (Emit e (return finish))
    where finish = Finish (fin r)

handleQM :: (CARD s)
         => (a -> IO ()) -- ^ Action to take on completion
         -> ManagerConn i s -- ^ Manager to handle if necessary
         -> HelpMe (Conref s) s (a, Effect s)
         -> IO ()
handleQM fin man h = handleQ' fin (getLatestVal man) h >>= \case
  Finish m -> m
  j -> giveJob j man

handleQR :: (CARD s)
         => ManagerConn i s
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
       => ManagerConn i s 
       -> (a -> IO ()) -- ^ Final action to execute on completion
       -> Carol s a -- ^ Operation to run, feeding the final action
       -> IO ()
runCarolM man fin t = handleQM fin man (runCarol' t)

-- | Run an operation, handing it off the store manager if it is not
-- trivial, and return the result to the original caller
runCarolR :: (CARD s)
       => ManagerConn i s
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

data Manager c s = Manager
  { manInbox :: TQueue (Either (BMsg (Feed c s)) (Job s IO ()))
  , manId :: PK
  , manFeedRoot :: FeedRoot s
  , manOthers :: [PK]
  , manJobQueue :: TQueue (Job s IO ())
  , manWaitingRoom :: [(Conref s, s -> IO (Job s IO ()))]
  , manCurrentJob :: Workspace (Job s IO ())
  , manLatest :: TVar (s, Store c PK s)
  , manRCIndex :: RCIndex PK s (Job s IO ())
  , grantMultiplex :: Int
  , batcheff :: [Effect s]
  , batchSize :: Int
  , onGetBroadcastCb :: IO ()
  , manDebugLevel :: Int }

type ManM r c s k = StateT (Manager c s) (CvRepCmd r (Store c PK s) k IO)

class (CvRDT r (Hist c PK s) IO, CARD s, CvChain r c (PK, Effect s) IO) => ManC r c s k
instance (CvRDT r (Hist c PK s) IO, CARD s, CvChain r c (PK, Effect s) IO) => ManC r c s k

data ManagerConn c s = ManagerConn 
  { eventQueue :: TQueue (Either (BMsg (Feed c s)) (Job s IO ()))
  , latestState :: TVar (s, Store c PK s)
  , manLoopThreadId :: ThreadId }

instance (CARD s) => CCarrier (ManagerConn c s) s IO where
  handleQ = handleQR
  handleQAsync c h fin = handleQM fin c h

stateStore :: Lens' (s, Store c i s) (Store c i s)
stateStore = _2

stateVal :: Lens' (s, Store c i s) s
stateVal = _1

giveUpdate :: BMsg (Feed c s) -> ManagerConn c s -> IO ()
giveUpdate m (ManagerConn q _ _) = lstm $ writeTQueue q (Left m)

giveJob :: Job s IO () -> ManagerConn c s -> IO ()
giveJob j (ManagerConn q _ _) = lstm $ writeTQueue q (Right j)

getLatestState :: ManagerConn c s -> IO (s, Store c PK s)
getLatestState (ManagerConn _ v _) = readTVarIO v

getLatestVal :: ManagerConn c s -> IO s
getLatestVal m = (^.stateVal) <$> getLatestState m

getLatestStore :: ManagerConn c s -> IO (Store c PK s)
getLatestStore m = (^.stateStore) <$> getLatestState m

killManager :: ManagerConn c s -> IO (s, Store c PK s)
killManager conn@(ManagerConn _ _ i) = do 
  sFinal <- getLatestState conn
  killThread i
  return sFinal

-- | Microseconds to seconds
ms2s :: (Fractional a) => Int -> a
ms2s ms = fromIntegral ms / 1000000

data DManagerSettings c s = DManagerSettings
  { timeoutUnitSize :: Int
  , setBatchSize :: Int
  , onUpdate :: (s, Store c PK s) -> IO ()
  , onValUpdate :: s -> IO ()
  , onLockUpdate :: Locks PK s -> IO ()
  , onGetBroadcast :: IO ()
  , dmsDebugLevel :: Int }

-- | Default settings using 'mempty' for the base store value.
defaultDManagerSettings :: (Monoid s) => DManagerSettings c s
defaultDManagerSettings = defaultDManagerSettings' mempty

-- | Default settings with an explicitly provided base store value.
defaultDManagerSettings' :: s -> DManagerSettings c s
defaultDManagerSettings' s = DManagerSettings
  { timeoutUnitSize = 100000
  , setBatchSize = 1
  , onUpdate = const $ return ()
  , onValUpdate = const $ return ()
  , onLockUpdate = const $ return ()
  , onGetBroadcast = return ()
  , dmsDebugLevel = 0 }

-- | A tag to tell awaitNetwork whether it is continuing in online (a
-- first message was recieved) or offline (the timeout was reached)
-- mode
data Proceed = PrMessage | PrTimeout

-- | Create a delay action that waits for either the first remote
-- message to arrive at the replica or for the optional timeout (in
-- microseconds).  The return value of the await action is 'Just' @i@
-- if a message has been received (from node @i@) and 'Nothing' if a
-- timeout has been reached.
awaitNetwork :: DManagerSettings c s 
             -> Maybe Int 
             -> IO (DManagerSettings c s, IO Bool)
awaitNetwork dms timeout = do
  tv <- newTVarIO Nothing
  let open pr = lstm $ readTVar tv >>= \case
                         Nothing -> swapTVar tv (Just pr) >> return ()
                         _ -> return ()

      await = do case timeout of
                   Just ms -> do forkIO $ threadDelay ms >> open PrTimeout
                                 return ()
                   Nothing -> return ()
                 lstm $ readTVar tv >>= \case
                          Just PrMessage -> return True
                          Just PrTimeout -> return False
                          Nothing -> retry

      dms' = dms { onGetBroadcast = onGetBroadcast dms >> open PrMessage }
  return (dms', await)

-- | Initialize a replica manager.  This returns a 'TQueue' for
-- updates from other replicas and jobs from other threads, and a
-- 'TVar' which can be read to get the latest calculated store value.
initManager :: (Ord s, ManC r c s ())
            => PK -- ^ This replica's ID
            -> [PK] -- ^ Other replicas' IDs
            -> Phone (Feed c s) -- ^ Communication interface
            -> r -- ^ Event graph resolver 
            -> Feed c s -- ^ Initial feed (ID + store)
            -> DManagerSettings c s
            -> IO (ManagerConn c s)
initManager i os phone r (froot@(FeedRoot _ val0),store0) (DManagerSettings ts bsize upCb upCbVal upCbLocks msgCb dbl) = do
  eventQueue <- newTQueueIO
  jobQueue <- newTQueueIO
  val1 <- evalHist r val0 (store0^.hist) mempty
  latestState <- newTVarIO (val1,store0)
  rci <- newRCIndex (ms2s ts)
  let manager = Manager
        eventQueue
        i
        froot
        os
        jobQueue
        []
        Idle
        latestState
        rci
        0
        []
        bsize
        msgCb
        dbl
      onUp = upWithSumms latestState upCb upCbVal upCbLocks r val0
  ti <- forkIO $ do
          runCvRep
            r
            store0
            (Map.fromList [(store0^.hist,val1)])
            (\s -> broadcast phone (froot,s) >> return ())
            onUp 
            (runStateT (dbg 1 "Starting loop..." 
                        >> managerLoop) manager)
          return ()
  return $ ManagerConn eventQueue latestState ti

dbg :: (ManC r c s k) => Int -> String -> ManM r c s k ()
dbg d s = do man <- get
             if manDebugLevel man >= d
                then liftIO $ putStrLn s >> hFlush stdout
                else return ()

managerLoop :: (ManC r c s k) => ManM r c s k ()
managerLoop = do
  -- Handle latest event (incorp update or enque job)
  -- liftIO $ putStrLn "Handle latest..."
  dbg 1 "Loop Phase: handleLatest"
  handleLatest
  -- Put jobs ready for retry back in the job queue
  -- liftIO $ putStrLn "Resurrect fails..."
  dbg 1 "Loop Phase: resurrectFails"
  resurrectFails
  -- Work on current job or start next one, taking first from the
  -- waiting area.  New jobs that need requests have the request made
  -- and go to the waiting area.
  -- liftIO $ putStrLn "Work..."
  dbg 1 "Loop Phase: workOnJob"
  workOnJob
  -- Enque locking requests from other replicas
  -- liftIO $ putStrLn "Handle lock reqs..."
  dbg 1 "Loop Phase: handleLockReqs"
  handleLockReqs
  -- And grant them at the appropriate rate
  -- liftIO $ putStrLn "Grant lock reqs..."
  dbg 1 "Loop Phase: grantLockReqs"
  grantLockReqs
  -- And loop
  managerLoop

onCurrent :: (ManC r c s k) 
          => (Workspace (Job s IO ()) -> Workspace (Job s IO ())) 
          -> ManM r c s k ()
onCurrent f = modify (\m -> m { manCurrentJob = f (manCurrentJob m) })

onRCI :: (ManC r c s k) 
      => (RCIndex PK s (Job s IO ()) -> IO (RCIndex PK s (Job s IO ())))
      -> ManM r c s k ()
onRCI f = do rci <- manRCIndex <$> get
             rci' <- liftIO (f rci)
             modify (\m -> m {manRCIndex = rci' })

data ManEvent c s = ManNew (Either (BMsg (Feed c s)) 
                                   (Job s IO ())) 
                  | ManRate (RCIndex PK s (Job s IO ())) 

handleLatest :: (ManC r c s k) => ManM r c s k ()
handleLatest = do
  man <- get
  let updates = (ManRate <$> awaitTimeouts (manRCIndex man))
                <|> (ManNew <$> readTQueue (manInbox man)) 
  lstm updates >>= \case
    ManNew (Right j) -> case j of
      _ -> do dbg 1 "Received job."
              lstm $ writeTQueue (manJobQueue man) j
    ManNew (Left (BCast (_,s) ack)) -> do
      dbg 1 "Received broadcast."
      lift (incorp s) >>= \case
        Just _ -> liftIO (ack True) >> dbg 1 "Rebroadcast."
        Nothing -> liftIO (ack False)
      liftIO (onGetBroadcastCb man)
    ManNew (Left (SReq respond)) -> do
      s <- lift check
      liftIO $ respond (manFeedRoot man,s)
      dbg 1 "Responded to state request."
    ManRate rci' -> do
      modify (\m -> m { manRCIndex = rci' })


getWaiting :: (ManC r c s k) => ManM r c s k (Maybe (Job s IO ()))
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

anyWaiting :: (ManC r c s k) => ManM r c s k Bool
anyWaiting = manWaitingRoom <$> get >>= \case
    [] -> return False
    _ -> return True

manGetLatest :: (ManC r c s k) => ManM r c s k s
manGetLatest = (^.stateVal) <$> (lstm . readTVar =<< manLatest <$> get)

putOnHold :: (ManC r c s k) => Conref s -> (s -> IO (Job s IO ())) -> ManM r c s k ()
putOnHold c fj = do
  i <- manId <$> get
  lift (emitOn' locks $ return . request i c)
  modify (\m -> m { manWaitingRoom = (c,fj) : manWaitingRoom m })

histAppend' :: (ManC r c s k) => Effect s -> ManM r c s k ()
histAppend' e = do
  i <- manId <$> get
  lift $ histAppend i e

sendBatch :: (ManC r c s k) => ManM r c s k ()
sendBatch = do
  beff <- batcheff <$> get
  if length beff > 0
     then do histAppend' (fold beff)
             modify $ \m -> m { batcheff = [] }
     else return ()

enbatch :: (ManC r c s k) => Effect s -> ManM r c s k ()
enbatch e = do
  bsize <- batchSize <$> get
  beff' <- (e:) . batcheff <$> get
  modify $ \m -> m { batcheff = beff' }
  if length beff' >= bsize
     then sendBatch
     else return ()

workOnJob :: (ManC r c s k) => ManM r c s k ()
workOnJob = manCurrentJob <$> get >>= \case

  Working j0 j -> do
    i <- manId <$> get
    others <- manOthers <$> get
    ls <- lift.use $ store.locks
    case j of -- do work!
      Request c f -> do
        -- liftIO $ putStrLn "Handling nested request..."
        if not $ requested i c ls
           then lift (emitOn' locks $ return . request i c) >> return ()
           else if confirmed i others ls
                   then do advJob . f =<< manGetLatest
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
             then do -- liftIO (putStrLn "Releasing locks...")
                     mx <- grantMultiplex <$> get 
                     -- if mx > 1
                     --    then liftIO (putStrLn $ "Grant multiplex: " ++ show mx)
                     --    else return ()
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

advJob :: (ManC r c s k) => IO (Job s IO ()) -> ManM r c s k ()
advJob = (onCurrent . stepJob =<<) . liftIO

handleLockReqs :: (ManC r c s k) => ManM r c s k ()
handleLockReqs = do
  i <- manId <$> get
  rci <- manRCIndex <$> get
  ls <- lift.use $ store.locks
  rci' <- liftIO $ foldM (flip enqueGrant) rci (ungranted i ls)
  
  modify (\m -> m { manRCIndex = rci' })

grantLockReqs :: (ManC r c s k) => ManM r c s k ()
grantLockReqs = do
  man <- get
  liftIO (getGrant (manRCIndex man)) >>= \case
    Just ((i2,c),rci') -> do 
      sendBatch
      modify $ \m -> m { manRCIndex = rci' }
      lift . emitOn' locks $ \ls -> return (grant (manId man) i2 ls)
      -- liftIO $ putStrLn "Granted lock."
      return ()
    Nothing -> return ()

resurrectFails :: (ManC r c s k) => ManM r c s k ()
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

upWithSumms :: (Eq i, MonadIO m, Ord s, CARD s, Ord (Hist c i s), CvChain r c (i, Effect s) m)
            => TVar (s, Store c i s) -- ^ Location to post store
            -> ((s, Store c i s) -> m ()) -- ^ Main update callback
            -> (s -> m ()) -- ^ Val-only update callback
            -> (Locks i s -> m ()) -- ^ Locks-only update callback
            -> r -- ^ Resolver
            -> s -- ^ Base store value
            -> Store c i s -- ^ Locks + History to evaluate
            -> Map (Hist c i s) s -- ^ Summaries to use
            -> m (Map (Hist c i s) s)
upWithSumms stateV upCb upCbVal upCbLocks r valBase store' summs = do
  (val,store) <- lstm$ readTVar stateV
  val' <- evalHist r valBase (store'^.hist) summs
  lstm $ swapTVar stateV (val',store')
  upCb (val',store)
  if val /= val'
     then upCbVal val'
     else return ()
  if store^.locks /= store'^.locks
     then upCbLocks (store'^.locks)
     else return ()
  return (Map.insert (store'^.hist) val' summs)
