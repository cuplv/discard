{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

type BChan s g = ReaderT (TChan (g (Effect s)))

instance (MonadIO m, Store s) => MonadBCast s g (BChan s g m) where
  bcast g = do chan <- ask
               liftIO . atomically . writeTChan chan $ g

type RFace s g m a = ReaderT (TChan (Req s g m a))

rinvoke :: (MonadIO m) => FrOp s a -> RFace s g m a m a
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
joinChansIO f i o = do cin <- atomically $ dupTChan i
                       cout <- atomically $ dupTChan o
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

runNode :: (Ord i, MonadIO m, MonadEG g (Effect s) m)
        => i
        -> TChan (g (Effect s)) 
        -> RFace s g m a m a
        -> (m () -> IO ())
        -> m ()
runNode (rid :: i) (brc :: TChan (g (Effect s))) (act :: RFace s g m a m a) asIO = do 
  comm <- newTPair 
  let reqIter = (makeIter undefined) :: [m (Req s g m a)]
  liftIO . forkIO . asIO $ runReplica rid reqIter
  return ()
