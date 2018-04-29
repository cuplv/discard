{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.CARD.STM where

import Control.Monad.State
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.STM

import Data.EventGraph
import Data.EventGraph.SetEG
import Data.Effect
import Data.Effect.Common
import ConflictAware

type TChanCRDT g e m = ReaderT (TChan (g e)) m

instance (MonadIO m) => MonadBCast g e (TChanCRDT g e m) where
  bcast g = liftIO . atomically . (\c -> writeTChan c g) =<< ask

type TChanCARD g e m = ReaderT (TChan (Conref e)) (TChanCRDT g e m)

instance (MonadIO m) => MonadBCast g e (TChanCARD g e m) where
  bcast = lift . bcast

-- instance (MonadIO m) => MonadAudit e (TChanCARD g e m) where
--   audit k = liftIO . atomically . (\c -> writeTChan c k) =<< ask
