{-# LANGUAGE LambdaCase #-}

module CARD.LQ.Internal
  ( LQNode (..)
  , LQ
  , LQEnv
  , runLQ
  , runQuery
  , evalLQ
  , issue
  , query
  , module Control.Monad.Free

  ) where

import Control.Monad.Free
import Control.Monad.Reader
import Control.Monad.State

import CARD.Store

data LQNode s a = Issue (Effect s) a
                | Query (Conref s) (s -> a)

instance Functor (LQNode s) where
  fmap f m = case m of
    Issue e a -> Issue e (f a)
    Query c a -> Query c (\s -> f (a s))

type LQ s = Free (LQNode s)

type LQEnv s m = ReaderT (Conref s -> m s) (StateT (Effect s) m)

----------------------------------------------------------------------

runLQ :: (Monad m) => (Conref s -> m s) -> LQ s a -> m (a, Effect s)
runLQ runq t = runStateT (runReaderT (evalLQ t) runq) ef0

runQuery :: (Monad m) => Conref s -> LQEnv s m s
runQuery = (lift.lift =<<) . (ask <*>) . pure

evalLQ :: (Monad m) => LQ s a -> LQEnv s m a
evalLQ = \case
  Pure a -> return a
  Free (Issue e t) -> evalLQ t <* modify (e |<|)
  Free (Query c ft) -> evalLQ.ft =<< runQuery c

issue :: (Store s) => Effect s -> LQ s ()
issue e = Free (Issue e (Pure ()))

query :: (Store s) => Conref s -> LQ s s
query c = Free (Query c Pure)
