{-# LANGUAGE MultiParamTypeClasses #-}

module CARD.Network where

import CARD.Store

class (Ord i, Store s) => MonadBCast i s g m where
  bcast :: g (i, Effect s) -> m ()

class MonadAuditR m where
  release :: m ()

class (MonadAuditR m, AStore s) => MonadAudit s g m where
  audit :: Conref s -> m (Maybe (g (Effect s)))
