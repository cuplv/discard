{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module ConflictAware where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map

import Data.EventGraph
import Data.Effect

------------------------------------------------------------------------

type RID = String

data Replica g e = Replica { hist :: g e
                           , rid :: RID
                           , audits :: Map RID (Conref e) }

addR :: (EGMonad g e m, Effect e) => e -> Replica g e -> m (Replica g e)
addR e (Replica h r as) = Replica <$> (add e h) <*> pure r <*> pure as

evalHR :: (EGMonad g e m, Effect e) => Replica g e -> m (Store e)
evalHR = evalHistory . hist

class (Effect e, Monoid (Conref e)) => CEffect e where
  data Conref e
  blocks :: Conref e -> e -> Bool
  conEq :: Conref e

class (CEffect e) => MonadAudit e m where
  audit :: Conref e -> m Bool -- should return a Maybe EG instead
  release :: Conref e -> m ()

class MonadBCast g e m where
  bcast :: g e -> m ()

type StoreT g e m a = StateT (Replica g e) m a

modifyM :: (Monad m) => (s -> m s) -> StateT s m ()
modifyM f = put =<< (lift . f) =<< get

data LQTerm e a = Q (Conref e) (Store e -> LQTerm e a)
                | R a

instance Functor (LQTerm e) where
  fmap f t1 = case t1 of
                Q c ft2 -> Q c (fmap f . ft2)
                R a -> R (f a)
                
instance Applicative (LQTerm e) where
  pure = R
  (<*>) fa t1 = case fa of
                  Q c fa2 -> Q c ((<*> t1) . fa2)
                  R f -> fmap f t1

instance Monad (LQTerm e) where
  (>>=) t1 f = case t1 of
                 Q c ft2 -> Q c ((>>= f) . ft2)
                 R a -> f a

type Op e a = LQTerm e (e,a)

invoke :: (EGMonad g e m, MonadAudit e m, MonadBCast g e m)
       => Op e a 
       -> StoreT g e m (Maybe a)
invoke o = case o of
  R (e,a) -> modifyM (addR e) >> return (Just a)
  Q c f -> do resp <- lift $ audit c
              if resp
                 then invoke . f =<< lift.evalHR =<< get
                 else return Nothing
