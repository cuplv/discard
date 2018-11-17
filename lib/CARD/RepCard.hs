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
import System.IO (hFlush,stdout)
import Data.Time.Clock
import Data.Foldable (fold)

import CARD.CvRDT
import CARD.Network
import CARD.EventGraph
import CARD.Store
import CARD.Locks
import CARD.RepCore
import CARD.LQ
import CARD.RateControl

batchSize = 50 :: Int

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

data Work j = Work j j

-- In Working, the first j is the complete, unevaluated job which will
-- be reinstated upon a retry
data Workspace j = Working j j | Idle

initJob j Idle = Working j j

stepJob j (Working j0 _) = Working j0 j

failJob :: Workspace j -> Workspace j
failJob (Working j0 _) = Idle

finishJob (Working _ _) = Idle

data Manager i r s = Manager
  { manInbox :: TQueue (Either (BMsg (CardState i r s)) (Job s IO ()))
  , manId :: i
  , manOthers :: [i]
  , manJobQueue :: TQueue (Job s IO ())
  , manWaitingRoom :: [(Conref s, s -> IO (Job s IO ()))]
  , manCurrentJob :: Workspace (Job s IO ())
  , manLatest :: TVar s
  , manRCIndex :: RCIndex i s (Job s IO ())
  , grantMultiplex :: Int
  , batcher :: Effect s -> Bool
  , batcheff :: [Effect s]
  , myTurn :: Bool
  , turnTimer :: Maybe (TVar Bool) }

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

-- | Microseconds to seconds
ms2s :: (Fractional a) => Int -> a
ms2s ms = fromIntegral ms / 1000000

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
            -> (Effect s -> Bool) -- ^ Batching whitelist
            -> IO (ManagerConn i r s)
initManager i os ds r s0 ts bw = do
  eventQueue <- newTQueueIO
  jobQueue <- newTQueueIO
  latestState <- newTVarIO s0
  rci <- newRCIndex (ms2s ts)
  let turn = if and $ map (i <) os
                then True
                else False
      manager = Manager
        eventQueue
        i
        os
        jobQueue
        []
        Idle
        latestState
        rci
        0
        bw
        []
        turn
        Nothing
      onUp = upWithSumms latestState ((),r) s0
  ti <- forkIO $ do
          if turn
             then do putStrLn $ "My turn to start."
                     runCoreM ((),r) Map.empty ds onUp (runStateT (setTurnTimer >> managerLoop) manager)
             else runCoreM ((),r) Map.empty ds onUp (runStateT managerLoop manager)
          return ()
  return $ ManagerConn eventQueue latestState ti

managerLoop :: (ManC i r s k t) => ManM i r s k t ()
managerLoop = do
  -- takeTurn
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

-- takeTurn :: (ManC i r s k t) => ManM i r s k t ()
-- takeTurn = do
--   turn <- myTurn <$> get
--   i <- manId <$> get
--   locks <- fst <$> lift check
--   if turn && not (holding i locks)
--      then lift (emitFstOn $ return . request i c) >> return ()
--      else return ()

onCurrent :: (ManC i r s k t) 
          => (Workspace (Job s IO ()) -> Workspace (Job s IO ())) 
          -> ManM i r s k t ()
onCurrent f = modify (\m -> m { manCurrentJob = f (manCurrentJob m) })

onRCI :: (ManC i r s k t) 
      => (RCIndex i s (Job s IO ()) -> IO (RCIndex i s (Job s IO ())))
      -> ManM i r s k t ()
onRCI f = do rci <- manRCIndex <$> get
             rci' <- liftIO (f rci)
             modify (\m -> m {manRCIndex = rci' })

data ManEvent i r s = ManNew (Either (BMsg (CardState i r s)) 
                                     (Job s IO ())) 
                    | ManRate (RCIndex i s (Job s IO ()))
                    | ManPass

awaitTurnTimer :: (ManC i r s k t) => ManM i r s k t (STM (ManEvent i r s))
awaitTurnTimer = do
  turnTimer <$> get >>= \case
    Just tt -> return $ const ManPass <$> (STM.check =<< readTVar tt)
    Nothing -> return $ const ManPass <$> (STM.check =<< return False)


setTurnTimer :: (ManC i r s k t) => ManM i r s k t ()
setTurnTimer = do 
  tt <- liftIO $ registerDelay 500000 -- 0.5s
  modify $ \m -> m { turnTimer = Just tt }

handleLatest :: (ManC i r s k t) => ManM i r s k t ()
handleLatest = do
  man <- get
  awtt <- awaitTurnTimer
  let updates = awtt
                <|> (ManRate <$> awaitTimeouts (manRCIndex man))
                <|> (ManNew <$> readTQueue (manInbox man))
  lstm updates >>= \case
    ManNew (Right j) -> case j of
      _ -> lstm $ writeTQueue (manJobQueue man) j
    ManNew (Left (BCast s)) -> do
      prestate <- fst <$> lift check
      lift (incorp s)
      poststate <- fst <$> lift check
      let [other] = manOthers man
          self = manId man
      if holding other prestate && not (holding other poststate) && prestate /= mempty
         then do liftIO $ putStrLn "My turn!"
                 modify $ \m -> m { myTurn = True }
                 wr <- manWaitingRoom <$> get
                 i <- manId <$> get
                 let c = foldr (\(c',_) c -> c' |&| c) crT wr
                 lift (emitFstOn $ return . request i c) >> return ()
                 setTurnTimer
         else return ()
    ManRate rci' -> do
      -- liftIO $ putStrLn "RateControl event."
      modify (\m -> m { manRCIndex = rci' })
    ManPass -> do
      liftIO $ putStrLn "Turn is finished."
      modify (\m -> m { myTurn = False })
      modify (\m -> m { turnTimer = Nothing })
      i <- manId <$> get
      let releaseAll = do
            locks <- fst <$> lift check
            if holding i locks
               then do liftIO (putStrLn "Releasing locks...")
                       mx <- grantMultiplex <$> get 
                       if mx > 1
                          then liftIO (putStrLn $ "Grant multiplex: " ++ show mx)
                          else return ()
                       modify (\m -> m { grantMultiplex = 0 })
                       lift (emitFstOn $ return . release i) >> return ()
               else do liftIO $ putStrLn "Taking and then releasing as signal >_<"
                       lift (emitFstOn $ return . request i crEqv) >> releaseAll
      releaseAll
      return ()


getWaiting :: (ManC i r s k t) => ManM i r s k t (Maybe (Job s IO ()))
getWaiting = do
  i <- manId <$> get
  others <- manOthers <$> get
  locks <- fst <$> lift check
  wr <- manWaitingRoom <$> get
  let findReady (c,fj) (Nothing,wr') = 
        if (holding' i locks `impl` c) && confirmed i others locks
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

anyWaiting :: (ManC i r s k t) => ManM i r s k t Bool
anyWaiting = manWaitingRoom <$> get >>= \case
    [] -> return False
    _ -> return True

manGetLatest :: (ManC i r s k t) => ManM i r s k t s
manGetLatest = lstm . readTVar =<< manLatest <$> get

putOnHold :: (ManC i r s k t) => Conref s -> (s -> IO (Job s IO ())) -> ManM i r s k t ()
putOnHold c fj = do
  i <- manId <$> get
  others <- manOthers <$> get
  locks <- fst <$> lift check
  turn <- myTurn <$> get
  if (not $ requested i c locks) && turn
     then lift (emitFstOn $ return . request i c) >> return ()
     else return ()
  modify (\m -> m { manWaitingRoom = (c,fj) : manWaitingRoom m })

sendBatch :: (ManC i r s k t) => ManM i r s k t ()
sendBatch = do
  i <- manId <$> get
  r2 <- snd <$> lift resolver
  beff <- batcheff <$> get
  if length beff > 0
     then do lift (emitSndOn (append r2 (i,fold beff)))
             modify $ \m -> m { batcheff = [] }
     else return ()

enbatch :: (ManC i r s k t) => Effect s -> ManM i r s k t ()
enbatch e = do
  beff' <- (e:) . batcheff <$> get
  modify $ \m -> m { batcheff = beff' }
  if length beff' >= batchSize
     then sendBatch
     else return ()

workOnJob :: (ManC i r s k t) => ManM i r s k t ()
workOnJob = manCurrentJob <$> get >>= \case

  Working j0 j -> do
    i <- manId <$> get
    others <- manOthers <$> get
    locks <- fst <$> lift check
    case j of -- do work!
      -- Request c f -> do
      --   liftIO $ putStrLn "Handling nested request..."
      --   if not $ requested i c locks
      --      then lift (emitFstOn $ return . request i c) >> return ()
      --      else if confirmed i others locks
      --              then do advJob . f =<< lstm . readTVar =<< manLatest <$> get
      --                      workOnJob
      --              else return ()
      Emit e m -> do
        case permitted' i e locks of
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
    locks <- fst <$> lift check
    -- let releaseAll = 
    --       if holding i locks
    --          then do liftIO (putStrLn "Releasing locks...")
    --                  mx <- grantMultiplex <$> get 
    --                  if mx > 1
    --                     then liftIO (putStrLn $ "Grant multiplex: " ++ show mx)
    --                     else return ()
    --                  modify (\m -> m { grantMultiplex = 0 })
    --                  lift (emitFstOn $ return . release i) >> return ()
    --          else return ()
    -- Try waiting room first
    getWaiting >>= \case
      Just j -> onCurrent (initJob j) >> workOnJob
      Nothing -> do
        -- If no one is ready in waiting room, take from main queue
        aw <- anyWaiting
        -- If no one is in the waiting room at all, release all locks
        -- if not aw
        --    then releaseAll
        --    else return ()
        manJobQueue <$> get >>= lstm . tryReadTQueue >>= \case
          -- Send initial requests to waiting room
          Just (Request c fj) -> putOnHold c fj >> workOnJob
          -- Anything else is good to go
          Just j -> onCurrent (initJob j) >> workOnJob
          -- If queue is empty, there is nothing left to do for now
          Nothing -> return ()

advJob :: (ManC i r s k t) => IO (Job s IO ()) -> ManM i r s k t ()
advJob = (onCurrent . stepJob =<<) . liftIO

handleLockReqs :: (ManC i r s k t) => ManM i r s k t ()
handleLockReqs = do
  i <- manId <$> get
  rci <- manRCIndex <$> get
  locks <- fst <$> lift check
  rci' <- liftIO $ foldM (flip enqueGrant) rci (ungranted i locks)
  
  modify (\m -> m { manRCIndex = rci' })

grantLockReqs :: (ManC i r s k t) => ManM i r s k t ()
grantLockReqs = do
  man <- get
  liftIO (getGrant (manRCIndex man)) >>= \case
    Just ((i2,c),rci') -> do 
      sendBatch
      modify $ \m -> m { manRCIndex = rci' }
      lift . emitFstOn $ \ls -> return (grant (manId man) i2 ls)
      -- liftIO $ putStrLn "Granted lock."
      return ()
    Nothing -> return ()

resurrectFails :: (ManC i r s k t) => ManM i r s k t ()
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
