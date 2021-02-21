{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lang.Carol.Internal
  ( CarolNode (..)
  , Carol
  , CarolEnv
  , Update
  , issued
  , produced
  , consumed
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
  , consume
  , produce
  , module Control.Monad.Free

  ) where

import System.Exit

import Control.Lens
import Control.Monad.Free
import Control.Monad.Reader
import Control.Monad.State

import Data.CARD

data CarolNode s a =
    Issue (Effect s) a
  | Query (Conref s) (s -> a)
  | Consume (Effect s) a
  | Produce (Effect s) a

instance Functor (CarolNode s) where
  fmap f m = case m of
    Issue e a -> Issue e (f a)
    Query c a -> Query c (\s -> f (a s))
    Consume e a -> Consume e (f a)
    Produce e a -> Produce e (f a)

type Carol s = Free (CarolNode s)

data Update s = Update
  { uIssued :: Effect s
  , uProduced :: Effect s
  , uConsumed :: Effect s }

instance Semigroup (Update s) where
  Update i1 p1 c1 <> Update i2 p2 c2 = 
    Update (i1 |>>| i2) (p1 |>>| p2) (c1 |>>| c2)

instance Monoid (Update s) where
  mempty = Update ef0 ef0 ef0

-- | An update's issued effect
issued :: Lens' (Update s) (Effect s)
issued = lens uIssued (\u e -> u { uIssued = e })

-- | An update's produced reservation
produced :: Lens' (Update s) (Effect s)
produced = lens uProduced (\u e -> u { uProduced = e })

-- | An update's consumed reservation
consumed :: Lens' (Update s) (Effect s)
consumed = lens uConsumed (\u e -> u { uConsumed = e })

type CarolEnv s m = ReaderT (Conref s -> m s) (StateT (Update s) m)

----------------------------------------------------------------------

-- | A "carrier" for a specific 'CARD'.
class (CARD s, Monad m) => CCarrier c s m where
  handleQ :: c -> HelpMe (Conref s) s (a, Update s) -> m a
  handleQAsync :: c -> HelpMe (Conref s) s (a, Update s) -> (a -> m ()) -> m ()

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

runCarol :: (Monad m) => (Conref s -> m s) -> Carol s a -> m (a, Update s)
runCarol runq t = runStateT (runReaderT (evalCarol t) runq) mempty

runCarol' :: Carol s a -> HelpMe (Conref s) s (a, Update s)
runCarol' = runCarol helpMe

runQuery :: (Monad m) => Conref s -> CarolEnv s m s
runQuery = (lift.lift =<<) . (ask <*>) . pure

evalCarol :: (Monad m) => Carol s a -> CarolEnv s m a
evalCarol = \case
  Pure a -> return a
  Free (Issue e t) -> evalCarol t <* (issued %= (e |<<|))
  Free (Query c ft) -> evalCarol.ft =<< runQuery c
  Free (Consume e t) -> evalCarol t <* (consumed %= (e |<<|))
  Free (Produce e t) -> evalCarol t <* (produced %= (e |<<|))

issue :: (CARD s) => Effect s -> Carol s ()
issue e = Free (Issue e (Pure ()))

query :: (CARD s) => Conref s -> Carol s s
query c = Free (Query c Pure)

queryT :: (CARD s) => Carol s s
queryT = query crT

consume :: (CARD s) => Effect s -> Carol s ()
consume e = Free (Consume e (Pure ()))

produce :: (CARD s) => Effect s -> Carol s ()
produce e = Free (Produce e (Pure ()))
