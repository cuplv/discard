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

import Data.CvRDT hiding (emit, emitOn, emitOn')
import Data.CARD.Store
import Lang.CCRT hiding (CCRT)
-- import Lang.Carol.Internal
-- import Network.Discard.RateControl
import Network.Discard.Broadcast

-- data Job c e s m a = 
--     -- | Issue the given effect.
--     Emit (Update e) (m (Job c e s m a))
--     -- | Request locks for the given conref.
--   | Request c (s -> m (Job c e s m a))
--     -- | Return the value to the caller.
--   | Finish (m a)
--     -- | Try to consume the given effect, passing it along if
--     -- successful.
--   | TryConsume e (Maybe e -> m (Job c e s m a))

type Job q i c e s = CCRT' q i c e s IO

-- data Job c e s
--   = Coordinate (CCRT c e s IO)

-- handleQ' :: (Monad m, Eq e, Monoid e, Eq c, Monoid c)
--          => (r -> m a) -- ^ Return callback
--          -> m s -- ^ Latest store ref
--          -> HelpMe c e s (r, Update e)
--          -> m (Job c e s m a)
-- handleQ' fin latest = \case
--   HelpMe c f
--     | c == uniC -> handleQ' fin latest . f =<< latest
--     | otherwise -> return (Request c (handleQ' fin latest . f))
--   GiveMe e f
--     | e == idE -> handleQ' fin latest (f (Just idE))
--     | otherwise -> return (TryConsume e (handleQ' fin latest . f))
--   GotIt (r,u)
--     |    u^.issued == idE
--       && u^.produced == idE
--       && u^.consumed == idE -> return finish
--     | otherwise -> return (Emit u (return finish))
--     where finish = Finish (fin r)

-- handleQM :: (Eq e, Eq c, Monoid e, Monoid c)
--          => (a -> IO ()) -- ^ Action to take on completion
--          -> ManagerConn q h i c e s -- ^ Manager to handle if necessary
--          -> HelpMe c e s (a, Update e)
--          -> IO ()
-- handleQM fin man h = handleQ' fin (getLatestVal man) h >>= \case
--   Finish m -> m
--   j -> giveJob j man

-- handleQR :: (Eq e, Eq c, Monoid e, Monoid c)
--          => ManagerConn q h i c e s
--          -> HelpMe c e s (a, Update e)
--          -> IO a
-- handleQR man h = do
--   tmv <- newEmptyTMVarIO
--   let fin a = lstm (putTMVar tmv a) >> return ()
--   handleQM fin man h
--   lstm $ takeTMVar tmv

-- -- | Run an operation, handing it off the store manager if it is not
-- -- trivial, and perform some final action when it is complete.  This
-- -- action may be performed on either the calling thread or the manager
-- -- thread.
-- runCarolM :: (Eq e, Eq c, Monoid e, Monoid c)
--           => ManagerConn q h i c e s
--           -> (a -> IO ()) -- ^ Final action to execute on completion
--           -> Carol c e s a -- ^ Operation to run, feeding the final action
--           -> IO ()
-- runCarolM man fin t = handleQM fin man (runCarol' t)

-- -- | Run an operation, handing it off the store manager if it is not
-- -- trivial, and return the result to the original caller
-- runCarolR :: (Eq e, Eq c, Monoid e, Monoid c)
--           => ManagerConn q h i c e s
--           -> Carol c e s a
--           -> IO a
-- runCarolR man t = handleQR man (runCarol' t)

-- -- In Working, the first j is the complete, unevaluated job which will
-- -- be reinstated upon a retry
-- data Workspace j = Working j j | Idle

-- initJob j Idle = Working j j

-- stepJob j (Working j0 _) = Working j0 j

-- failJob :: Workspace j -> Workspace j
-- failJob (Working j0 _) = Idle

-- finishJob (Working _ _) = Idle

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

-- instance (Eq e, Eq c, Monoid e, Monoid c) => CCarrier (ManagerConn q h i c e s) c e s IO where
--   handleQ = handleQR
--   handleQAsync c h fin = handleQM fin c h

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
  , dmsReqHandler :: ReqHandler q i c
  }

-- | Default settings using 'mempty' for the base store value.
defaultDManagerSettings :: (Monoid s) => DManagerSettings q h i c e s
defaultDManagerSettings = defaultDManagerSettings' mempty

-- | Default settings with an explicitly provided base store value.
defaultDManagerSettings' :: s -> DManagerSettings q h i c e s
defaultDManagerSettings' s = DManagerSettings
  { timeoutUnitSize = 100000
  , setBatchSize = 1
  , onUpdate = const $ return ()
  , onValUpdate = const $ return ()
  , onLockUpdate = const $ return ()
  , onGetBroadcast = return ()
  , baseStoreValue = s
  , dmsDebugLevel = 0
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
initManager i os ds r val0 store0 (DManagerSettings ts bsize upCb upCbVal upCbLocks msgCb valBase dbl rqh) = do
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

  -- -- Put jobs ready for retry back in the job queue
  -- -- liftIO $ putStrLn "Resurrect fails..."
  -- dbg 1 "Loop Phase: resurrectFails"
  -- resurrectFails

  dbg 1 "Loop Phase: handleWaiting"
  handleWaiting

  -- -- Work on current job or start next one, taking first from the
  -- -- waiting area.  New jobs that need requests have the request made
  -- -- and go to the waiting area.
  -- -- liftIO $ putStrLn "Work..."
  -- dbg 1 "Loop Phase: workOnJob"
  -- workOnJob
  
  -- Run the application-specific request-handling function on the
  -- request state.
  dbg 1 "Loop Phase: handleRequests"
  handleRequests

  -- -- Enque locking requests from other replicas
  -- -- liftIO $ putStrLn "Handle lock reqs..."
  -- dbg 1 "Loop Phase: handleLockReqs"
  -- handleLockReqs
  -- -- And grant them at the appropriate rate
  -- -- liftIO $ putStrLn "Grant lock reqs..."
  -- dbg 1 "Loop Phase: grantLockReqs"
  -- grantLockReqs

  -- And loop
  managerLoop

-- onCurrent :: (ManC r q h i c e s) 
--           => (Workspace (Job q i c e s) -> Workspace (Job q i c e s)) 
--           -> ManM r q h i c e s k ()
-- onCurrent f = modify (\m -> m { manCurrentJob = f (manCurrentJob m) })

-- onRCI :: (ManC r q h i c e s) 
--       => (RCIndex i c (Job c e s IO ()) 
--           -> IO (RCIndex i c (Job c e s IO ())))
--       -> ManM r q h i c e s k ()
-- onRCI f = do rci <- manRCIndex <$> get
--              rci' <- liftIO (f rci)
--              modify (\m -> m {manRCIndex = rci' })

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
    -- ManNew (Right t) -> case t of
    --   _ -> do dbg 1 "Received job."
    --           lstm $ writeTQueue (manJobQueue man) j
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
    -- ManRate rci' -> do
    --   -- liftIO $ putStrLn "RateControl event."
    --   modify (\m -> m { manRCIndex = rci' })

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
  h <- manReqHandler <$> get
  r <- lift.use $ store.rqs
  cf <- lift.use $ store.caps
  lift $ incorp' caps (h r cf)
  return ()


-- getWaiting :: (ManC r q h i c e s)
--            => ManM r q h i c e s k (Maybe (Job q i c e s))
-- getWaiting = do
--   i <- manId <$> get
--   others <- manOthers <$> get
--   ls <- lift.use $ store.locks
--   wr <- manWaitingRoom <$> get
--   let findReady (c,fj) (Nothing,wr') = 
--         if (holding' i ls `impl` c) && confirmed i others ls
--            then (Just (c,fj),wr')
--            else (Nothing, (c,fj):wr')
--       findReady cfj (fj,wr') = (fj,cfj:wr')
--       (r,wr') = foldr findReady (Nothing,[]) wr
--   case r of
--     Just (c,fj) -> do
--       modify (\m -> m { manWaitingRoom = wr' })
--       modify (\m -> m { grantMultiplex = grantMultiplex m + 1 })
--       Just <$> (liftIO . fj =<< manGetLatest' c)
--     Nothing -> return Nothing

anyWaiting :: (ManC r q h i c e s) => ManM r q h i c e s k Bool
anyWaiting = manWaitingRoom <$> get >>= \case
    [] -> return False
    _ -> return True

manGetLatest :: (ManC r q h i c e s) => ManM r q h i c e s k s
manGetLatest = (^.stateVal) <$> (lstm . readTVar =<< manLatest <$> get)

-- -- | Get latest store value, weakened by the worst-case view of
-- -- reservations according to the supplied conref.
-- manGetLatest' :: (ManC r q h i c e s) => c -> ManM r q h i c e s k s
-- manGetLatest' c = do
--   s <- lstm . readTVar =<< manLatest <$> get
--   let re = resWX c (s ^. stateStore . ress)
--       v = eFun re (s ^. stateVal)
--   return v

putOnHold :: (ManC r q h i c e s) => Job q i c e s -> ManM r q h i c e s k ()
putOnHold t = modify (\m -> m { manWaitingRoom = manWaitingRoom m ++ [t] })

-- putOnHold :: (ManC r q h i c e s)
--           => c
--           -> (s -> IO (Job c e s))
--           -> ManM r q h i c e s k ()
-- putOnHold c fj = do
--   i <- manId <$> get
--   lift (emitOn' locks $ return . request i c)
--   modify (\m -> m { manWaitingRoom = (c,fj) : manWaitingRoom m })

-- histAppend' :: (ManC r q h i c e s) => e -> ManM r q h i c e s k ()
-- histAppend' e = do
--   i <- manId <$> get
--   lift $ histAppend i e

-- sendBatch :: (ManC r q h i c e s) => ManM r q h i c e s k ()
-- sendBatch = do
--   beff <- batcheff <$> get
--   if length beff > 0
--      then do histAppend' (fold beff)
--              modify $ \m -> m { batcheff = [] }
--      else return ()

-- enbatch :: (ManC r q h i c e s) => e -> ManM r q h i c e s k ()
-- enbatch e = do
--   bsize <- batchSize <$> get
--   beff' <- (e:) . batcheff <$> get
--   modify $ \m -> m { batcheff = beff' }
--   if length beff' >= bsize
--      then sendBatch
--      else return ()

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

-- workOnJob :: (ManC r q h i c e s) => ManM r q h i c e s k ()
-- workOnJob = manCurrentJob <$> get >>= \case

--   Working j0 j -> do
--     i <- manId <$> get
--     others <- manOthers <$> get
--     cf <- lift.use $ store.caps
--     case j of -- do work!
--       Coordinate t -> do
--         case checkRW i t cf of
--           Just sim -> do snap <- eFun sim <$> manGetLatest
--                          e <- liftIO $ transactT t snap
--                          enbatch e
--           Nothing -> case makeRequest t cf of
--             Just cf' -> do lift (emit' caps cf')
--                            return ()
--             Nothing -> liftIO $ failT t
        -- if not $ requested i c cs
        --    then undefined
        --    else undefined
      -- Request c f -> do
      --   -- liftIO $ putStrLn "Handling nested request..."
      --   if not $ requested i c ls
      --      then lift (emitOn' locks $ return . request i c) >> return ()
      --      else if confirmed i others ls
      --              then do advJob . f =<< manGetLatest' c
      --                      workOnJob
      --              else return ()
      -- TryConsume e f -> do
      --   r <- lift.use $ store.ress
      --   case consumeRes i e r of
      --     Just r' -> do lift $ incorp' ress r'
      --                   advJob (f $ Just e)
      --     Nothing -> advJob (f Nothing)
      --   workOnJob
      -- Emit u m -> do
      --   let e = u^.issued
      --       cond = permitted' i 
      --                         (u^.consumed) 
      --                         ((u^.produced) <> (u^.issued)) 
      --                         ls
      --   case cond of
      --     Right () -> do
      --       r <- lift.use $ store.ress
      --       -- Produce reservations.  The idea is to split whatever
      --       -- has been produced among all known replica IDs
      --       -- (including self).  For the absolute simplest version,
      --       -- we also want to "atomize" the reservations into unit
      --       -- effects so that we can simply match them against
      --       -- consumed unit effects.
      --       if (u^.produced) /= idE
      --          then let newRs = zip (i:others)
      --                               (effectPartition
      --                                  (length others + 1)
      --                                  (u^.produced))
      --                   r' = produceRes i newRs r
      --               in lift $ incorp' ress r' >> return ()
      --          else return ()
      --       -- liftIO $ putStrLn "Got past produce"
      --       enbatch e
      --       rci <- manRCIndex <$> get
      --       if effectLe (getRCBlocker rci) idE e
      --          then resurrectFails
      --          else return ()
      --       advJob m
      --       workOnJob
      --     Left c -> do
      --       -- Return consumed reservations to local pool.  Note that
      --       -- current version merges what might have before been
      --       -- distinct reservations into a single effect.
      --       if (u^.consumed) /= idE
      --          then do r <- lift.use $ store.ress
      --                  lift $ incorp' ress (produceRes i [(i,u^.consumed)] r)
      --                  return ()
      --          else return ()
      --       onCurrent failJob
      -- Finish m -> do 
      --   -- Perform finishing callback
      --   liftIO m
      --   onCurrent finishJob
      --   workOnJob

  -- Idle -> do -- No current job, try to pop from queue
  --   i <- manId <$> get
  --   ls <- lift.use $ store.locks
  --   let releaseAll = 
  --         if holding i ls
  --            then do -- liftIO (putStrLn "Releasing locks...")
  --                    mx <- grantMultiplex <$> get 
  --                    -- if mx > 1
  --                    --    then liftIO (putStrLn $ "Grant multiplex: " ++ show mx)
  --                    --    else return ()
  --                    modify (\m -> m { grantMultiplex = 0 })
  --                    lift (emitOn' locks $ return . release i) >> return ()
  --            else return ()
  --   -- Try waiting room first
  --   getWaiting >>= \case
  --     Just j -> onCurrent (initJob j) >> workOnJob
  --     Nothing -> do
  --       -- If no one is ready in waiting room, take from main queue
  --       aw <- anyWaiting
  --       -- If no one is in the waiting room at all, release all locks
  --       if not aw
  --          then releaseAll
  --          else return ()
  --       manJobQueue <$> get >>= lstm . tryReadTQueue >>= \case
  --         -- -- Send initial requests to waiting room
  --         -- Just (Request c fj) -> putOnHold c fj >> workOnJob
  --         -- Anything else is good to go
  --         Just j -> onCurrent (initJob j) >> workOnJob
  --         -- If queue is empty, there is nothing left to do for now
  --         Nothing -> return ()

-- advJob :: (ManC r q h i c e s) => IO (Job q i c e s) -> ManM r q h i c e s k ()
-- advJob = (onCurrent . stepJob =<<) . liftIO

-- handleLockReqs :: (ManC r q h i c e s) => ManM r q h i c e s k ()
-- handleLockReqs = do
--   i <- manId <$> get
--   rci <- manRCIndex <$> get
--   ls <- lift.use $ store.locks
--   -- rci' <- liftIO $ foldM (flip enqueGrant) rci (ungranted i ls)
  
--   -- modify (\m -> m { manRCIndex = rci' })

-- grantLockReqs :: (ManC r q h i c e s) => ManM r q h i c e s k ()
-- grantLockReqs = do
--   man <- get
--   liftIO (getGrant (manRCIndex man)) >>= \case
--     Just ((i2,c),rci') -> do 
--       sendBatch
--       -- modify $ \m -> m { manRCIndex = rci' }
--       lift . emitOn' locks $ \ls -> return (grant (manId man) i2 ls)
--       -- liftIO $ putStrLn "Granted lock."
--       return ()
--     Nothing -> return ()

-- resurrectFails :: (ManC r q h i c e s) => ManM r q h i c e s k ()
-- resurrectFails = do
--   man <- get
--   liftIO (getRetry (manRCIndex man)) >>= \case
--     Just (j,rci') -> 
--       do case j of
--            Request c fj -> putOnHold c fj
--            _ -> lstm $ writeTQueue (manJobQueue man) j
--          modify (\m -> m { manRCIndex = rci' })
--          resurrectFails
--     Nothing -> return ()

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
  upCb (val',store)
  if val /= val'
     then upCbVal val'
     else return ()
  if store^.caps /= store'^.caps
     then upCbLocks (store'^.caps)
     else return ()
  return (Map.insert (store'^.hist) val' summs)
