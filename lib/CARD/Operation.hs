{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module CARD.Operation where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Lens

import CARD.Store
import CARD.EventGraph
import CARD.Network

data LTerm s a = LTerm (s -> a)

data LQTerm s a = Q (Conref s) (LTerm s (LQTerm s a))
                | R a

-- | Conflict-free store operation
type FrOp s a = LTerm s (Effect s, a)

-- | Conflict-aware store operation
type AwOp s a = LQTerm s (Effect s, a)


------------------------------------------------------------------------

data FrRep i g s = FR { _frRepID :: i
                      , _frHist :: g (Effect s) }
makeLenses ''FrRep

data AwRep i g s = AR { _awInnerRep :: FrRep i g s
                      , _awAudits :: Map i (Conref s) }

makeLenses ''AwRep

class (Ord (RepID r), Store (RepStore r), EventGraph (RepEG r) (Effect (RepStore r))) => Rep r where
  type RepID r
  type RepEG r :: * -> *
  type RepStore r
  repID :: Lens' r (RepID r)
  hist :: Lens' r ((RepEG r) (Effect (RepStore r)))

instance (Ord i, Store s, EventGraph g (Effect s)) => Rep (FrRep i g s) where
  type RepID (FrRep i g s) = i
  type RepEG (FrRep i g s) = g
  type RepStore (FrRep i g s) = s
  repID = frRepID -- lens _frRepID (\(FR i1 h) i2 -> FR i2 h)
  hist = frHist

instance (Ord i, Store s, EventGraph g (Effect s)) => Rep (AwRep i g s) where
  type RepID (AwRep i g s) = i
  type RepEG (AwRep i g s) = g
  type RepStore (AwRep i g s) = s
  repID = awInnerRep . repID
  hist = awInnerRep . hist

deliver :: (MonadEG g (Effect s) m, Rep r, RepEG r ~ g, RepStore r ~ s) 
        => g (Effect s) 
        -> StateT r m ()
deliver g1 = do r <- get
                h2 <- lift (merge g1 (view hist r))
                put (set hist h2 r)

modifyM :: (Monad m) => (s -> m s) -> StateT s m ()
modifyM f = put =<< (lift . f) =<< get

invoke :: (MonadEG g (Effect s) m, Rep r, RepEG r ~ g, RepStore r ~ s) 
       => FrOp s a
       -> StateT r m a
invoke (LTerm o) = do h1 <- view hist <$> get
                      (e,a) <- o <$> (lift $ evalHistory h1)
                      h2 <- lift $ append e h1
                      modify (set hist h2)
                      return a

history :: (MonadEG g (Effect s) m, Rep r, RepEG r ~ g, RepStore r ~ s) 
        => StateT r m (g (Effect s))
history = view hist <$> get


------------------------------------------------------------------------

data Req s g m a = Delivery (g (Effect s)) | Command (FrOp s a) (a -> m ())

runReplica :: (MonadEG g (Effect s) m, MonadBCast s g m, Ord i)
           => i
           -> [m (Req s g m a)] 
           -> m ()
runReplica i rs = evalStateT program initRep
  where program = lift (sequence rs) >>= mapM_ handle
        handle r = case r of
                     Delivery g -> deliver g
                     Command o cb -> invoke o >>= lift . cb
        initRep = FR i empty
