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
  , giveMe
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

data CarolNode c e s a =
    Issue e a
  | Query c (s -> a)
  | Consume e (Maybe e -> a)
  | Produce e a

instance Functor (CarolNode c e s) where
  fmap f m = case m of
    Issue e a -> Issue e (f a)
    Query c a -> Query c (\s -> f (a s))
    Consume e a -> Consume e (\m -> f (a m))
    Produce e a -> Produce e (f a)

type Carol c e s = Free (CarolNode c e s)

data Update e = Update
  { uIssued :: e
  , uProduced :: e
  , uConsumed :: e }

instance (Semigroup e) => Semigroup (Update e) where
  Update i1 p1 c1 <> Update i2 p2 c2 = 
    Update (i2 <> i1) (p2 <> p1) (c2 <> c1)

instance (Monoid e) => Monoid (Update e) where
  mempty = Update idE idE idE

-- | An update's issued effect
issued :: Lens' (Update e) e
issued = lens uIssued (\u e -> u { uIssued = e })

-- | An update's produced reservation
produced :: Lens' (Update e) e
produced = lens uProduced (\u e -> u { uProduced = e })

-- | An update's consumed reservation
consumed :: Lens' (Update e) e
consumed = lens uConsumed (\u e -> u { uConsumed = e })

type CarolEnv c e s m
  = ReaderT (c -> m s, e -> m (Maybe e)) (StateT (Update e) m)

----------------------------------------------------------------------

-- | A "carrier" for a specific 'CARD'.
class (CARD s, Monad m) => CCarrier r c e s m where
  handleQ :: r -> HelpMe c e s (a, Update e) -> m a
  handleQAsync :: r -> HelpMe c e s (a, Update e) -> (a -> m ()) -> m ()

-- | Run a Carol operation using the provided carrier.
carol :: (CCarrier r c s m) => r -> Carol c e s a -> m a
carol r t = handleQ r (runCarol' t)

-- | Run a Carol operation in the background, taking a particular
-- action after it terminates.
carolAsync :: (CCarrier r c e s m) 
           => r
           -> Carol c e s a 
           -> (a -> m ()) -- ^ Action to perform after operation
                          -- terminates
           -> m ()
carolAsync r t fin = handleQAsync r (runCarol' t) fin

-- | Run a Carol operation in the background, with no post-termination
-- action
carolAsync' :: (CCarrier r c e s m) => r -> Carol c e s a -> m ()
carolAsync' r t = handleQAsync r (runCarol' t) (const $ return ())

----------------------------------------------------------------------

data HelpMe c e s a =
    HelpMe c (s -> HelpMe c e s a)
    -- | Helper term for Consume
  | GiveMe e (Maybe e -> HelpMe c e s a)
  | GotIt a

instance Functor (HelpMe c e s) where
  fmap f (HelpMe c g) = HelpMe c (fmap f . g)
  fmap f (GiveMe e g) = GiveMe e (fmap f . g)
  fmap f (GotIt a) = GotIt (f a)

instance Applicative (HelpMe c e s) where
  pure = GotIt
  (<*>) (HelpMe c f) a = HelpMe c (\s -> f s <*> a)
  (<*>) (GiveMe e f) a = GiveMe e (\m -> f m <*> a)
  (<*>) (GotIt f) a = fmap f a

instance Monad (HelpMe c e s) where
  (>>=) (HelpMe c a) f = HelpMe c (\s -> a s >>= f)
  (>>=) (GiveMe e a) f = GiveMe e (\m -> a m >>= f)
  (>>=) (GotIt a) f = f a

helpMe :: c -> HelpMe c e s s
helpMe c = HelpMe c return

giveMe :: e -> HelpMe c e s (Maybe e)
giveMe e = GiveMe e return

staticApp :: s -> HelpMe c e s a -> a
staticApp s = \case
  HelpMe _ f -> staticApp s (f s)
  GiveMe _ f -> staticApp s (f Nothing)
  GotIt a -> a

runCarol :: (Monad m)
  => (c -> m s)
  -> (e -> m (Maybe e))
  -> Carol c e s a
  -> m (a, Update e)
runCarol runq runc t = runStateT (runReaderT (evalCarol t) (runq,runc)) mempty

runCarol' :: Carol c e s a -> HelpMe c e s (a, Update e)
runCarol' = runCarol helpMe giveMe

runQuery :: (Monad m) => c -> CarolEnv c e s m s
runQuery = (lift.lift =<<) . ((fst <$> ask) <*>) . pure

runConsume :: (Monad m) => e -> CarolEnv c e s m (Maybe e)
runConsume = (lift.lift =<<) . ((snd <$> ask) <*>) . pure

evalCarol :: (Monad m) => Carol c e s a -> CarolEnv c e s m a
evalCarol = \case
  Pure a -> return a
  Free (Issue e t) -> evalCarol t <* (issued %= (e <>))
  Free (Query c ft) -> evalCarol.ft =<< runQuery c
  Free (Consume e ft) -> do
    me <- runConsume e
    case me of
      Just e' -> consumed %= (e' <>)
      Nothing -> return ()
    evalCarol (ft me)
  Free (Produce e t) -> evalCarol t <* (produced %= (e <>))

issue :: e -> Carol c e s ()
issue e = Free (Issue e (Pure ()))

query :: c -> Carol c e s s
query c = Free (Query c Pure)

queryT :: (Monoid c) => Carol c e s s
queryT = query uniC

consume :: e -> Carol c e s (Maybe e)
consume e = Free (Consume e Pure)

produce :: e -> Carol s e s ()
produce e = Free (Produce e (Pure ()))
