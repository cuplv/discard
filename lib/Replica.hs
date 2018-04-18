module Replica where

import Control.Monad.State

import Data.EventGraph

data Replica g e = Replica { hist :: g e }

deliver :: (EventGraph g, Ord e) 
        => g e 
        -> Replica g e 
        -> Resolver g (Replica g e)
deliver g1 (Replica g2) = Replica <$> merge g1 g2

type Op e a = Store e -> (e,a)

invoke :: (EventGraph g, Ord e, Effect e) 
       => Op e a 
       -> Replica g e 
       -> Resolver g (Replica g e, g e, a)
invoke o (Replica g1) = do v <- eval <$> toList g1
                           let (e,a) = o v
                           g2 <- add e g1
                           return (Replica g2,g2,a)

------------------------------------------------------------------------

type RepS g e = StateT (Replica g e) (Resolver g)

deliverM :: (EventGraph g, Ord e) => g e -> RepS g e ()
deliverM g1 = (put . Replica) =<< (lift . merge g1 . hist) =<< get

invokeM :: (EventGraph g, Ord e, Effect e) => Op e a -> RepS g e (g e,a)
invokeM o = do (r',g',a) <- (lift . invoke o) =<< get
               put r'
               return (g',a)
