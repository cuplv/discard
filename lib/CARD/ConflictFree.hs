{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module ConflictFree 
  ( Replica
  , Op
  , StoreT
  , deliver
  , invoke
  , history
  , initRep
  ) where

import Control.Monad.State

import Data.EventGraph
import Data.Effect

------------------------------------------------------------------------

data Replica g e = Replica { hist :: g e }

type Op e a = Store e -> (e,a)

type StoreT g e m a = StateT (Replica g e) m a

deliver :: (EGMonad g e m) => g e -> StoreT g e m ()
deliver g1 = (put . Replica) =<< (lift . merge g1 . hist) =<< get

modifyM :: (Monad m) => (s -> m s) -> StateT s m ()
modifyM f = put =<< (lift . f) =<< get

addr :: (EGMonad g e m, Effect e) => e -> Replica g e -> m (Replica g e)
addr e r = Replica <$> (add e (hist r))

invoke :: (EGMonad g e m, Effect e) => Op e a -> StoreT g e m a
invoke o = do (e,a) <- o <$> (lift . evalHistory . hist =<< get)
              modifyM (addr e)
              return a

history :: (EGMonad g e m, Effect e) => StoreT g e m (g e)
history = hist <$> get

initRep :: (EG g e) => Replica g e
initRep = Replica empty
