{-# LANGUAGE LambdaCase #-}

module Lang.Carol.Internal
  ( CarolNode (..)
  , Carol
  , CarolEnv
  , HelpMe (..)
  , helpMe
  , staticApp
  , runCarol
  , runCarol'
  , runQuery
  , evalCarol
  , issue
  , query
  , module Control.Monad.Free

  ) where

import System.Exit

import Control.Monad.Free
import Control.Monad.Reader
import Control.Monad.State

import Data.CARD

data CarolNode s a = Issue (Effect s) a
                | Query (Conref s) (s -> a)

instance Functor (CarolNode s) where
  fmap f m = case m of
    Issue e a -> Issue e (f a)
    Query c a -> Query c (\s -> f (a s))

type Carol s = Free (CarolNode s)

type CarolEnv s m = ReaderT (Conref s -> m s) (StateT (Effect s) m)

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

runCarol :: (Monad m) => (Conref s -> m s) -> Carol s a -> m (a, Effect s)
runCarol runq t = runStateT (runReaderT (evalCarol t) runq) ef0

runCarol' :: Carol s a -> HelpMe (Conref s) s (a, Effect s)
runCarol' = runCarol helpMe

runQuery :: (Monad m) => Conref s -> CarolEnv s m s
runQuery = (lift.lift =<<) . (ask <*>) . pure

evalCarol :: (Monad m) => Carol s a -> CarolEnv s m a
evalCarol = \case
  Pure a -> return a
  Free (Issue e t) -> evalCarol t <* modify (e |<<|)
  Free (Query c ft) -> evalCarol.ft =<< runQuery c

issue :: (CARD s) => Effect s -> Carol s ()
issue e = Free (Issue e (Pure ()))

query :: (CARD s) => Conref s -> Carol s s
query c = Free (Query c Pure)
