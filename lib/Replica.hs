{-# LANGUAGE TypeFamilies #-}

module Replica where

import Control.Monad.State

import Data.EventGraph

data Replica g e = Replica { hist :: g e }

class Monoid e => Effect e where
  type Store e
  eval :: [e] -> Store e

deliver :: (EventGraph g, Ord e, EGMonad m g)
        => g e 
        -> Replica g e 
        -> m (Replica g e)
deliver g1 (Replica g2) = Replica <$> merge g1 g2

type Op e a = Store e -> (e,a)

invoke :: (EventGraph g, Ord e, Effect e, EGMonad m g)
       => Op e a 
       -> Replica g e 
       -> m (Replica g e, g e, a)
invoke o (Replica g1) = do v <- eval <$> toList g1
                           let (e,a) = o v
                           g2 <- add e g1
                           return (Replica g2,g2,a)

------------------------------------------------------------------------

type RepS g e = StateT (Replica g e)

deliverM :: (EventGraph g, Ord e, EGMonad m g) => g e -> RepS g e m ()
deliverM g1 = (put . Replica) =<< (lift . merge g1 . hist) =<< get

invokeM :: (EventGraph g, Ord e, Effect e, EGMonad m g) => Op e a -> RepS g e m (g e,a)
invokeM o = do (r',g',a) <- (lift . invoke o) =<< get
               put r'
               return (g',a)
