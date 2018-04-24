{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Replica where

import Control.Monad.State

import Data.EventGraph

data Replica g e = Replica { hist :: g e }

class (Monoid e, Monoid (Store e)) => Effect e where
  data Store e
  runEffect :: Store e -> e -> Store e

evalHist :: (EGMonad g e m, Effect e) => Replica g e -> m (Store e)
evalHist = foldg runEffect mempty . hist

deliver :: (EGMonad g e m)
        => g e 
        -> Replica g e 
        -> m (Replica g e)
deliver g1 (Replica g2) = Replica <$> merge g1 g2

type Op e a = Store e -> (e,a)

invoke :: (Effect e, EGMonad g e m)
       => Op e a 
       -> Replica g e 
       -> m (Replica g e, g e, a)
invoke o (Replica g1) = do v <- evalHist (Replica g1)
                           let (e,a) = o v
                           g2 <- add e g1
                           return (Replica g2,g2,a)

------------------------------------------------------------------------

type RepS g e = StateT (Replica g e)

deliverM :: (EGMonad g e m) => g e -> RepS g e m ()
deliverM g1 = (put . Replica) =<< (lift . merge g1 . hist) =<< get

invokeM :: (Effect e, EGMonad g e m) => Op e a -> RepS g e m (g e,a)
invokeM o = do (r',g',a) <- (lift . invoke o) =<< get
               put r'
               return (g',a)
