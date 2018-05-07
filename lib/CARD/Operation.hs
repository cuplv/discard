-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

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

-- | An event for store type 's', marked with a replica ID of type
-- 'i'.
type SEv i s = (i, Effect s)

-- | Conflict-free replica state
data FrRep i g s = FR { _frRepID :: i
                      , _frHist :: g (i, Effect s) }
makeLenses ''FrRep

-- | Conflict-aware replica state
data AwRep i g s = AR { _awInnerRep :: FrRep i g s
                      , _awAudits :: Map i (Conref s) }
makeLenses ''AwRep

-- | Event type for 'r'
type REv r = (RepID r, Effect (RepStore r))
-- | Event graph type for 'r'
type REg r = (RepEG r) (REv r)

-- | Basic class of replicas
class (Ord (RepID r),
       Ord (REg r),
       Store (RepStore r), 
       EventGraph (RepEG r) (REv r)) => Replica r where
  -- | The type of ID used to sort and identify replicas
  type RepID r
  -- | The backing store for the replica's event graph
  type RepEG r :: * -> *
  -- | The type of store that the replica manages
  type RepStore r
  repID :: Lens' r (RepID r)
  hist :: Lens' r ((RepEG r) (RepID r, Effect (RepStore r)))

instance (Ord i,
          Ord (g (SEv i s)),
          Store s,
          EventGraph g (SEv i s)) => Replica (FrRep i g s) where
  type RepID (FrRep i g s) = i
  type RepEG (FrRep i g s) = g
  type RepStore (FrRep i g s) = s
  repID = frRepID -- lens _frRepID (\(FR i1 h) i2 -> FR i2 h)
  hist = frHist

instance (Ord i,
          Ord (g (SEv i s)),
          Store s,
          EventGraph g (SEv i s)) => Replica (AwRep i g s) where
  type RepID (AwRep i g s) = i
  type RepEG (AwRep i g s) = g
  type RepStore (AwRep i g s) = s
  repID = awInnerRep . repID
  hist = awInnerRep . hist

-- | Shorthand constraint for a 'Replica' 'r' which can run in backing
-- monad 'm'.
class (Replica r, MonadEG (RepEG r) (REv r) m) => Rep r m
instance (Replica r, MonadEG (RepEG r) (REv r) m) => Rep r m

deliver :: (Rep r m)
        => REg r -- ^ Event graph to deliver
        -> StateT r m ()
deliver g1 = do r <- get
                h2 <- lift (merge g1 (view hist r))
                put (set hist h2 r)

invoke :: (Rep r m)
       => FrOp (RepStore r) a -- ^ Operation to invoke
       -> StateT r m (Bool,a)
invoke (LTerm o) = do h1 <- view hist <$> get
                      rid <- view repID <$> get
                      let evalHistory = foldg (\s (_,e) -> runEffect s e) initStore
                      (e,a) <- o <$> (lift $ evalHistory h1)
                      h2 <- lift $ append (rid,e) h1
                      modify (set hist h2)
                      let change = case e of
                                     Effect [] -> False
                                     _ -> True
                      return (change,a)

history :: (Rep r m)
        => StateT r m (REg r)
history = view hist <$> get


------------------------------------------------------------------------

-- | A system event for a replica to process
data Req r m a = 
  -- | An event graph to be merged from another replica
  Delivery (RepID r) (REg r)
  -- | An invocation to perform an operation
  | Command (FrOp (RepStore r) a) (a -> m ())

runReplica :: (Rep r m, RepID r ~ i, MonadBCast i (RepStore r) (RepEG r) m)
           => i -- ^ This replica's ID
           -> m (Req r m a) -- ^ Request producer
           -> m ()
runReplica i rsIter = evalStateT loop initRep
  where loop = lift rsIter >>= handle >> loop
        handle r = case r of
                     Delivery is g -> 
                       if is /= i
                          then do i <- view repID <$> get
                                  deliver g 
                          else return ()
                     Command o cb -> do (change,a) <- invoke o
                                        lift . cb $ a
                                        if change
                                           then lift . bcast =<< view hist <$> get
                                           else return ()

        initRep = FR i empty
