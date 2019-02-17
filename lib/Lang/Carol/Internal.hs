{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lang.Carol.Internal
  ( CarolNode (..)
  , Carol
  , CarolEnv
  , CCarrier (..)
  , carol
  , carolAsync
  , carolAsync'
  , HelpMe (..)
  , helpMe
  , staticApp
  , runCarol
  , runCarol'
  , runQuery
  , evalCarol
  , issue
  , query
  , queryT
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

-- | A "carrier" for a specific 'CARD'.
class (CARD s, Monad m) => CCarrier c s m where
  handleQ :: c -> HelpMe (Conref s) s (a, Effect s) -> m a
  handleQAsync :: c -> HelpMe (Conref s) s (a, Effect s) -> (a -> m ()) -> m ()

-- | Run a Carol operation using the provided carrier.
carol :: (CCarrier c s m) => c -> Carol s a -> m a
carol c t = handleQ c (runCarol' t)

-- | Run a Carol operation in the background, taking a particular
-- action after it terminates.
carolAsync :: (CCarrier c s m) 
           => c 
           -> Carol s a 
           -> (a -> m ()) -- ^ Action to perform after operation
                          -- terminates
           -> m ()
carolAsync c t fin = handleQAsync c (runCarol' t) fin

-- | Run a Carol operation in the background, with no post-termination
-- action
carolAsync' :: (CCarrier c s m) => c -> Carol s a -> m ()
carolAsync' c t = handleQAsync c (runCarol' t) (const $ return ())

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

queryT :: (CARD s) => Carol s s
queryT = query crT
