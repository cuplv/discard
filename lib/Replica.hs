module Replica where

import Data.EventGraph

data Replica g e = Replica (g e)

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
