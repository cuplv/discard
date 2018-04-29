{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module CARD.STM where

import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.STM

import CARD.Store
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
