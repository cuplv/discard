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

import System.Exit

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

data HelpMe c r a = HelpMe c (r -> HelpMe c r a) | GotIt a

instance Functor (HelpMe c r) where
  fmap f (HelpMe c g) = HelpMe c (fmap f . g)
  fmap f (GotIt a) = GotIt (f a)

instance Applicative (HelpMe c r) where
  pure = GotIt
  (<*>) (HelpMe c f) a = HelpMe c (\r -> f r <*> a)
  (<*>) (GotIt f) a = fmap f a

instance Monad (HelpMe c r) where
  (>>=) (HelpMe c a) f = HelpMe c (\r -> a r >>= f)
  (>>=) (GotIt a) f = f a

helpMe :: c -> HelpMe c r r
helpMe c = HelpMe c return

staticApp :: r -> HelpMe c r a -> a
staticApp r = \case
  HelpMe _ f -> staticApp r (f r)
  GotIt a -> a

staticStore :: s -> LQ s a -> (a, Effect s)
staticStore s t = staticApp s $ runLQ helpMe t

runLQ :: (Monad m) => (Conref s -> m s) -> LQ s a -> m (a, Effect s)
runLQ runq t = runStateT (runReaderT (evalLQ t) runq) ef0

runLQ' :: LQ s a -> HelpMe (Conref s) s (a, Effect s)
runLQ' = runLQ helpMe

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