{-# LANGUAGE LambdaCase #-}

module Lang.Carol.Internal
  ( LQNode (..)
  , LQ
  , LQEnv
  , HelpMe (..)
  , helpMe
  , staticApp
  , runLQ
  , runLQ'
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
import Control.Monad.Cont

import Data.CARD

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

data EvalQ s a = EvalQ (Conref s) (s -> LQ s a)

evalCPS :: (Monad m) => Effect s -> LQ s a -> ContT (a, Effect s) m (EvalQ s a)
evalCPS e term = case term of
  Pure a -> ContT (const (return (a,e)))
  Free (Issue e' t) -> evalCPS (e' |<| e) t
  Free (Query c ft) -> mapContT (\r -> do (a,e') <- r
                                          return (a, e' |<| e)) 
                                (ContT (\h -> h (EvalQ c ft)))
