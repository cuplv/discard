{-# LANGUAGE MultiParamTypeClasses #-}

module CARD.Network where

import CARD.Store

class (Store s) => MonadBCast s g m where
  bcast :: g (Effect s) -> m ()

class MonadAuditR m where
  release :: m ()

class (MonadAuditR m, AStore s) => MonadAudit s g m where
  audit :: Conref s -> m (Maybe (g (Effect s)))
