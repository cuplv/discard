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

data FrRep i g s = FR { _frRepID :: i
                      , _frHist :: g (i, Effect s) }
makeLenses ''FrRep

data AwRep i g s = AR { _awInnerRep :: FrRep i g s
                      , _awAudits :: Map i (Conref s) }

makeLenses ''AwRep

class (Ord (RepID r), Store (RepStore r), EventGraph (RepEG r) (RepID r, Effect (RepStore r))) => Rep r where
  type RepID r
  type RepEG r :: * -> *
  type RepStore r
  repID :: Lens' r (RepID r)
  hist :: Lens' r ((RepEG r) (RepID r, Effect (RepStore r)))

instance (Ord i, Store s, EventGraph g (i, Effect s)) => Rep (FrRep i g s) where
  type RepID (FrRep i g s) = i
  type RepEG (FrRep i g s) = g
  type RepStore (FrRep i g s) = s
  repID = frRepID -- lens _frRepID (\(FR i1 h) i2 -> FR i2 h)
  hist = frHist

instance (Ord i, Store s, EventGraph g (i, Effect s)) => Rep (AwRep i g s) where
  type RepID (AwRep i g s) = i
  type RepEG (AwRep i g s) = g
  type RepStore (AwRep i g s) = s
  repID = awInnerRep . repID
  hist = awInnerRep . hist

deliver :: (MonadEG g (RepID r, Effect s) m, Rep r, RepEG r ~ g, RepStore r ~ s) 
        => g (RepID r, Effect s) 
        -> StateT r m ()
deliver g1 = do r <- get
                h2 <- lift (merge g1 (view hist r))
                put (set hist h2 r)

modifyM :: (Monad m) => (s -> m s) -> StateT s m ()
modifyM f = put =<< (lift . f) =<< get

invoke :: (MonadEG g (RepID r, Effect s) m, Rep r, RepEG r ~ g, RepStore r ~ s) 
       => FrOp s a
       -> StateT r m (Bool, a)
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

history :: (MonadEG g (RepID r, Effect s) m, Rep r, RepEG r ~ g, RepStore r ~ s) 
        => StateT r m (g (RepID r, Effect s))
history = view hist <$> get


------------------------------------------------------------------------

data Req i s g m a = Delivery i (g (i, Effect s)) | Command (FrOp s a) (a -> m ())

runReplica :: (MonadIO m, MonadEG g (i, Effect s) m, MonadBCast i s g m, Ord i, Show i)
           => i
           -> m (Req i s g m a)
           -> m ()
runReplica i rsIter = evalStateT loop initRep
  where loop = lift rsIter >>= handle >> loop
        handle r = case r of
                     Delivery is g -> 
                       if is /= i
                          then do i <- view repID <$> get
                                  -- liftIO . putStrLn $ (show i ++ ": handling delivery...")
                                  deliver g 
                          else return ()
                     Command o cb -> do (change,a) <- invoke o
                                        lift . cb $ a
                                        if change
                                           then lift . bcast =<< view hist <$> get
                                           else return ()

        initRep = FR i empty
