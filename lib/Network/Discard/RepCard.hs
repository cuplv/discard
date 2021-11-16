{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Network.Discard.RepCard 
  ( Job (..)
  , runCCRT
  , ManC
  , ManagerConn (..)
  , DManagerSettings (..)
  , initManager
  , defaultDManagerSettings'
  , awaitNetwork
  , giveUpdate
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

import Data.CvRDT hiding (emit, emitOn, emitOn')
import Data.CARD.Store
import Lang.CCRT hiding (CCRT)
import Network.Discard.Broadcast

type Job q i c e s = CCRT' q i c e s IO

data Manager q h i c e s = Manager
  { manInbox :: TQueue (Either (BMsg (Store q h i c e)) (Job q i c e s))
  , manId :: i
  , manOthers :: [i]
  , manJobQueue :: TQueue (Job q i c e s)
  , manWaitingRoom :: [Job q i c e s]
  , manLatest :: TVar (s, Store q h i c e)
  , grantMultiplex :: Int
  , batched :: Int
  , batchMax :: Int
  , onGetBroadcastCb :: IO ()
  , manDebugLevel :: Int
  , manReqHandler :: ReqHandler q i c
  }

type ManM r q h i c e s k
  = StateT
      (Manager q h i c e s)
      (CvRepCmd r (Store q h i c e) k IO)

class
  (CvRDT r q IO, CvRDT r (Hist h i e) IO, Ord i, CvChain r h (i,e) IO, Eq e, Eq c, Ord e, Ord c, Cap c e, EffectDom e s) => ManC r q h i c e s
instance
  (CvRDT r q IO, CvRDT r (Hist h i e) IO, Ord i, CvChain r h (i,e) IO, Eq e, Eq c, Ord e, Ord c, Cap c e, EffectDom e s) => ManC r q h i c e s

data ManagerConn q h i c e s = ManagerConn 
  { eventQueue :: TQueue (Either (BMsg (Store q h i c e)) (Job q i c e s))
  , latestState :: TVar (s, Store q h i c e)
  , manLoopThreadId :: ThreadId }

runCCRT :: ManagerConn q h i c e s -> CCRT' q i c e s IO -> IO ()
runCCRT man t = giveJob t man

stateStore :: Lens' (s, Store q h i c e) (Store q h i c e)
stateStore = _2

stateVal :: Lens' (s, Store q h i c e) s
stateVal = _1

giveUpdate :: BMsg (Store q h i c e) -> ManagerConn q h i c e s -> IO ()
giveUpdate m (ManagerConn q _ _) = lstm $ writeTQueue q (Left m)

giveJob :: Job q i c e s -> ManagerConn q h i c e s -> IO ()
giveJob j (ManagerConn q _ _) = lstm $ writeTQueue q (Right j)

getLatestState :: ManagerConn q h i c e s -> IO (s, Store q h i c e)
getLatestState (ManagerConn _ v _) = readTVarIO v

getLatestVal :: ManagerConn q h i c e s -> IO s
getLatestVal m = (^.stateVal) <$> getLatestState m

getLatestStore :: ManagerConn q h i c e s -> IO (Store q h i c e)
getLatestStore m = (^.stateStore) <$> getLatestState m

killManager :: ManagerConn q h i c e s -> IO (s, Store q h i c e)
killManager conn@(ManagerConn _ _ i) = do
  sFinal <- getLatestState conn
  killThread i
  return sFinal

-- | Microseconds to seconds
ms2s :: (Fractional a) => Int -> a
ms2s ms = fromIntegral ms / 1000000

data DManagerSettings q h i c e s = DManagerSettings
  { timeoutUnitSize :: Int
  , setBatchSize :: Int
  , onUpdate :: (s, Store q h i c e) -> IO ()
  , onValUpdate :: s -> IO ()
  , onLockUpdate :: Capconf i c -> IO ()
  , onGetBroadcast :: IO ()
  , baseStoreValue :: s
  , dmsDebugLevel :: Int
  , dmsBaseCapconf :: Capconf i c
  , dmsBaseReqState :: q
  , dmsReqHandler :: ReqHandler q i c
  }

-- | Default settings with an explicitly provided base store value.
defaultDManagerSettings'
  :: q
  -> Capconf i c
  -> s
  -> DManagerSettings q h i c e s
defaultDManagerSettings' q cf s = DManagerSettings
  { timeoutUnitSize = 100000
  , setBatchSize = 1
  , onUpdate = const $ return ()
  , onValUpdate = const $ return ()
  , onLockUpdate = const $ return ()
  , onGetBroadcast = return ()
  , baseStoreValue = s
  , dmsDebugLevel = 0
  , dmsBaseCapconf = cf
  , dmsBaseReqState = q
  , dmsReqHandler = emptyReqHandler 
  }

-- | A tag to tell awaitNetwork whether it is continuing in online (a
-- first message was recieved) or offline (the timeout was reached)
-- mode
data Proceed = PrMessage | PrTimeout

-- | Create a delay action that waits for either the first remote
-- message to arrive at the replica or for the optional timeout (in
-- microseconds).  The return value of the await action is 'Just' @i@
-- if a message has been received (from node @i@) and 'Nothing' if a
-- timeout has been reached.
awaitNetwork :: DManagerSettings q h i c e s
             -> Maybe Int 
             -> IO (DManagerSettings q h i c e s, IO Bool)
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

-- | Initialize a replica manager.
initManager :: (Ord s, ManC r q h i c e s, Transport t, Carries t (Store q h i c e), Res t ~ IO)
            => i -- ^ This replica's ID
            -> [i] -- ^ Other replicas' IDs
            -> [Dest t] -- ^ Broadcast targets
            -> r -- ^ Event graph resolver 
            -> s -- ^ Initial store value
            -> Store q h i c e -- ^ Initial history + locks
            -> DManagerSettings q h i c e s
            -> IO (ManagerConn q h i c e s)
initManager i os ds r val0 store0 (DManagerSettings ts bsize upCb upCbVal upCbLocks msgCb valBase dbl _ _ rqh) = do
  eventQueue <- newTQueueIO
  jobQueue <- newTQueueIO
  latestState <- newTVarIO (val0,store0)
  let bc = broadcast ds
      manager = Manager
        eventQueue
        i
        os
        jobQueue
        []
        latestState
        0
        0
        bsize
        msgCb
        dbl
        rqh
      onUp = upWithSumms latestState upCb upCbVal upCbLocks r valBase
  ti <- forkIO $ do
          runCvRep
            r
            store0
            (Map.fromList [(store0^.hist,val0)])
            bc
            onUp 
            (runStateT (doHellos ds msgCb 
                        >> dbg 1 "Starting loop..." 
                        >> managerLoop) manager)
          return ()
  return $ ManagerConn eventQueue latestState ti

doHellos :: (ManC r q h i c e s, Transport t, Carries t (Store q h i c e), Res t ~ IO) 
         => [Dest t] 
         -> IO ()
         -> ManM r q h i c e s k ()
doHellos ds cbB = do
  ups <- lift . lift $ helloAll ds
  lift $ mapM_ incorp ups
  case ups of
    [] -> return ()
    _ -> liftIO cbB
  lift bcast

dbg :: (ManC r q h i c e s) => Int -> String -> ManM r q h i c e s k ()
dbg d s = do man <- get
             if manDebugLevel man >= d
                then liftIO $ putStrLn s >> hFlush stdout
                else return ()

managerLoop :: (ManC r q h i c e s) => ManM r q h i c e s k ()
managerLoop = do
  -- Handle latest event (incorp update or enque job)
  -- liftIO $ putStrLn "Handle latest..."
  dbg 1 "Loop Phase: handleLatest"
  handleLatest

  dbg 1 "Loop Phase: handleWaiting"
  handleWaiting

  -- Run the application-specific request-handling function on the
  -- request state.
  dbg 1 "Loop Phase: handleRequests"
  handleRequests

  -- And loop
  managerLoop

data ManEvent q h i c e s = ManNew (Either (BMsg (Store q h i c e)) 
                                           (Job q i c e s)) 

handleLatest :: (ManC r q h i c e s) => ManM r q h i c e s k ()
handleLatest = do
  man <- get
  let updates = ManNew <$> readTQueue (manInbox man)
  lstm updates >>= \case
    ManNew (Right t) -> tryTransact t >>= \case
      Nothing -> return () -- all done, t has passed or failed
      Just u -> tryRequest u t >>= \case
        False -> return () -- all done, t has failed
        True -> putOnHold t -- awaiting a response, resume later
    ManNew (Left (BCast s)) -> do
      dbg 1 "Received broadcast."
      -- If the received broadcast contains new updates, rebroadcast it.
      lift (incorp s) >>= \case
        Just _ -> lift bcast >> dbg 1 "Rebroadcast."
        Nothing -> return ()
      liftIO (onGetBroadcastCb man)
    ManNew (Left Hello) ->
      -- -- Respond to a 'Hello' by broadcasting the current state,
      -- -- bringing the new node up to date.
      -- lift bcast >> liftIO (onGetBroadcastCb man)
      dbg 1 "Received hello."

handleWaiting :: (ManC r q h i c e s) => ManM r q h i c e s k ()
handleWaiting = do
  i <- manId <$> get
  cf <- lift.use $ store.caps
  wr <- manWaitingRoom <$> get
  let tryT ts t = tryTransact t >>= \case
        Nothing -> return ts
        Just _ -> return $ ts ++ [t]
  wr' <- foldM tryT [] wr
  modify (\m -> m { manWaitingRoom = wr' })

handleRequests :: (ManC r q h i c e s) => ManM r q h i c e s k ()
handleRequests = do
  handle <- manReqHandler <$> get
  r <- lift.use $ store.rqs
  cf <- lift.use $ store.caps
  let (r',cf') = handle (r,cf)
  lift $ incorp' caps cf'
  lift $ incorp' rqs r
  if r /= r' || cf /= cf'
     then bcast'
     else return ()

anyWaiting :: (ManC r q h i c e s) => ManM r q h i c e s k Bool
anyWaiting = manWaitingRoom <$> get >>= \case
    [] -> return False
    _ -> return True

manGetLatest :: (ManC r q h i c e s) => ManM r q h i c e s k s
manGetLatest = (^.stateVal) <$> (lstm . readTVar =<< manLatest <$> get)

putOnHold :: (ManC r q h i c e s) => Job q i c e s -> ManM r q h i c e s k ()
putOnHold t = modify (\m -> m { manWaitingRoom = manWaitingRoom m ++ [t] })

-- | Broadcast current state and reset batch size.
bcast' :: (ManC r q h i c e s) => ManM r q h i c e s k ()
bcast' = do
  lift bcast
  modify $ \m -> m { batched = 0 }

-- | Append an effect to the local store.  If the max batch size has
-- been exceeded, or if there are no more transactions in the queue,
-- broadcast the change (resetting the batch size).
issueEffect :: (ManC r q h i c e s) => e -> ManM r q h i c e s k ()
issueEffect e = do
  i <- manId <$> get
  -- Append effect to local history
  lift $ histAppend i e
  bmax <- batchMax <$> get
  b' <- (+ 1) . batched <$> get
  qEmpty <- lstm . isEmptyTQueue =<< manJobQueue <$> get
  if b' > bmax || qEmpty
     then bcast'
     else modify $ \m -> m { batched = b' }

-- | Attempt to run a transaction, returning 'Nothing' if it has
-- completed or permanently failed, and @'Just' u@ if requirement @u@
-- remains unsatisfied.  If a requirement remains unsatisfied, the
-- caller should try to make a request on its behalf (if new), or
-- return it to the waiting room (if old).
tryTransact
  :: (ManC r q h i c e s)
  => Job q i c e s
  -> ManM r q h i c e s k (Maybe (Unsat c))
tryTransact t = do
  i <- manId <$> get
  cf <- lift.use $ store.caps
  case checkRW i t cf of
    Right sim -> do 
      snap <- eFun sim <$> manGetLatest
      e <- liftIO $ transactT t snap
      case consumeG i e cf of
        Just cf' -> do
          lift (incorp' caps cf')
          
          issueEffect e
          return Nothing
        Nothing -> do
          liftIO $ failT t (WriteError e)
          return Nothing
    Left u -> return (Just u)

-- | Attempt to coordinate on a transaction's behalf, returning 'True'
-- if a request has been made.  If the transaction has no coordination
-- strategy, fail the transaction and return 'False'.
tryRequest
  :: (ManC r q h i c e s)
  => Unsat c
  -> Job q i c e s
  -> ManM r q h i c e s k Bool
tryRequest u t = do
  q <- lift.use $ store.rqs
  case makeRequest t q of
    Just q' -> do
      lift (incorp' rqs q')
      bcast'
      return True
    Nothing -> do
      liftIO (failT t (UnsatError u))
      return False

lstm :: MonadIO m => STM a -> m a
lstm = liftIO . atomically

upWithSumms :: (Eq i, MonadIO m, Ord s, Ord (Hist h i e), CvRDT r q m, CvChain r h (i,e) m, Eq c, Ord c, EffectDom e s)
            => TVar (s, Store q h i c e) -- ^ Location to post store
            -> ((s, Store q h i c e) -> m ()) -- ^ Main update callback
            -> (s -> m ()) -- ^ Val-only update callback
            -> (Capconf i c -> m ()) -- ^ Caps-only update callback
            -> r -- ^ Resolver
            -> s -- ^ Base store value
            -> Store q h i c e -- ^ Caps + History to evaluate
            -> Map (Hist h i e) s -- ^ Summaries to use
            -> m (Map (Hist h i e) s)
upWithSumms stateV upCb upCbVal upCbLocks r valBase store' summs = do
  (val,store) <- lstm$ readTVar stateV
  val' <- evalHist r valBase (store'^.hist) summs
  lstm $ swapTVar stateV (val',store')
  upCb (val',store')
  if val /= val'
     then upCbVal val'
     else return ()
  if store^.caps /= store'^.caps
     then upCbLocks (store'^.caps)
     else return ()
  return (Map.insert (store'^.hist) val' summs)
