{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.LogStore where

-- | A rooted sequence, possibly stored through a monadic backend.
-- Here, @l@ is the log structure, @r@ is the root node type, @e@ is
-- the entry node type, and @m@ is the backend monad.
class LogStore l r e m where
  init :: r -> m (l r e)
  log :: e -> l r e -> m (l r e)
  unlog :: l r e -> m (Either r (e, l r e))

newtype ListLog r e = ListLog (r, [e]) deriving (Show,Read,Eq,Ord)

instance (Monad m) => LogStore ListLog r e m where
  init r = return $ ListLog (r,[])
  log e (ListLog (r,es)) = return $ ListLog (r,e:es)
  unlog (ListLog (r,e:es)) = return $ Right (e, ListLog (r,es))
  unlog (ListLog (r,[])) = return $ Left r
