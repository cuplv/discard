{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.LogStore where

import Data.CARD
import Data.LamportClock

type VClock = LClock

type Root i r = (i,d)

type Entry i e = (i, VClock i, e)

-- | A rooted sequence, possibly stored through a monadic backend.
-- Here, @l@ is the log structure, @i@ is the store/replica identity
-- type, @r@ is the root node type, @e@ is the entry node type, and
-- @m@ is the backend monad.
class LogStore l i r e m where
  init :: Root i r -> m (l i r e)
  log :: i -> e -> l i r e -> m (l i r e)
  unlog :: l i r e -> m (Either (Root i r) (Entry i e, l i r e))

-- newtype ListLog r e = ListLog (r, [e]) deriving (Show,Read,Eq,Ord)

-- instance (Monad m) => LogStore ListLog r e m where
--   init r = return $ ListLog (r,[])
--   log e (ListLog (r,es)) = return $ ListLog (r,e:es)
--   unlog (ListLog (r,e:es)) = return $ Right (e, ListLog (r,es))
--   unlog (ListLog (r,[])) = return $ Left r


-- -- | A CARD feed, where @l@ is the log store, @d@ is the CARD, @i@ is
-- -- the identifier type (for both feeds and replicas), and @m@ is the
-- -- backend monad.
-- class (LogStore l (i,d) (i, VClock i, Effect d) m) => Feed l d i m
