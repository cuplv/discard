{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE ScopedTypeVariables #-}

module CARD.STM where

import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.STM

import CARD.Store
import CARD.EventGraph
import CARD.Network
import CARD.Operation

type BChan i s g = ReaderT (TChan (g (i, Effect s)))

instance (Ord i, MonadIO m, Store s) => MonadBCast i s g (BChan i s g m) where
  bcast g = do chan <- ask
               liftIO . atomically . writeTChan chan $ g

type RFace r m a = ReaderT (TChan (Req r m a))

rinvoke :: (MonadIO m1, MonadIO m2) => FrOp (RepStore r) a -> RFace r m1 a m2 a
rinvoke o = do cbwrite <- liftIO newBroadcastTChanIO
               cbread <- liftIO . atomically $ dupTChan cbwrite
               invoker <- ask
               (liftIO 
                . atomically 
                . writeTChan invoker 
                $ Command o (liftIO . atomically . writeTChan cbwrite))
               liftIO . atomically $ readTChan cbread

-- | Join two channels end-to-end, with some transformation
joinChans :: (a -> b) -> (TChan a) -> (TChan b) -> IO ThreadId
joinChans f i o = joinChansIO (return . f) i o

-- | Join two channels end-to-end, performing some IO transformation
-- in between
joinChansIO :: (a -> IO b) -> (TChan a) -> (TChan b) -> IO ThreadId
joinChansIO f i cout = do cin <- atomically $ dupTChan i
                          let rc = do a <- atomically $ readTChan cin
                                      b <- f a
                                      atomically $ writeTChan cout b
                                      rc
                          forkIO rc

-- | Iteratively perform IO actions on items read from a channel
consume :: (a -> IO ()) -> (TChan a) -> IO ThreadId
consume f i = do cin <- atomically $ dupTChan i
                 let rc = (atomically $ readTChan cin) >>= f >> rc 
                 forkIO rc

makeIter :: (MonadIO m) => TChan a -> [m a]
makeIter c = (liftIO . atomically $ readTChan c) : makeIter c

newTPair :: (MonadIO m) => m (TChan a, TChan a)
newTPair = do cin <- liftIO newBroadcastTChanIO
              cout <- liftIO . atomically $ dupTChan cin
              return (cin,cout)

-- runNode :: (Show i, Ord i, MonadIO m, Store s, MonadEG g (i, Effect s) m)
--         => i
--         -> TChan (i, g (i, Effect s)) 
--         -> RFace i s g (BChan i s g m) b IO a
--         -> (m () -> IO ())
--         -> IO ()
runNode :: (MonadIO m, Rep r m, RepID r ~ i)
        => i
        -> TChan (i, REg r)
        -> RFace r (BChan i (RepStore r) (RepEG r) m) b IO a
        -> (m () -> IO ())
        -> IO ()
runNode rid brc act asIO = do 
  req <- liftIO newTChanIO -- Main request queue over which replica iterates
  liftIO $ joinChans (uncurry Delivery) brc req -- Broadcasts go on the request queue
  comm <- liftIO newTChanIO -- Commands from main thread will be sent here
  liftIO $ joinChans id comm req -- Commands go on the request queue
  let reqIter = liftIO . atomically . readTChan $ req
  brcPre <- liftIO newBroadcastTChanIO
  liftIO $ joinChans (\g -> (rid,g)) brcPre brc
  let repAction = runReplica rid reqIter
  let repRun = runReaderT repAction brcPre
  liftIO . forkIO . asIO $ repRun -- run replica
  runReaderT act comm -- run interactin script on main thread
  return ()
