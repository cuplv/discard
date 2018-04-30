{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module CARD.Store where

import Data.Foldable (foldl')
import Data.Set (Set)
import qualified Data.Set as Set

import CARD.EventGraph

class (Ord (Ef s)) => Store s where
  data Ef s
  initStore :: s
  defineEffect :: s -> Ef s -> s

class (Store s, Ord (Cr s)) => AStore s where
  data Cr s
  defineBlock :: Cr s -> Ef s -> Bool
  smartAnd :: (Set (Cr s)) -> Cr s -> Conref s

newtype Effect s = Effect [Ef s] 

deriving instance (Store s, Read (Ef s)) => Read (Effect s)
deriving instance (Store s, Show (Ef s)) => Show (Effect s)
deriving instance (Store s) => Eq (Effect s)
deriving instance (Store s) => Ord (Effect s)

runEffect :: (Store s) => s -> Effect s -> s
runEffect s (Effect es) = foldl' defineEffect s es

data Conref s = Conref (Set (Cr s)) | EQV

deriving instance (AStore s) => Eq (Conref s)
deriving instance (AStore s) => Ord (Conref s)

checkBlock :: (AStore s) => Conref s -> Effect s -> Bool
checkBlock (Conref cs) (Effect es) = or (defineBlock <$> (Set.toList cs) <*> es)
checkBlock EQV (Effect es) = case es of
                               [] -> False
                               _ -> True

-- evalHistory :: (MonadEG g (Effect s) m, Store s) => g (Effect s) -> m s
-- evalHistory = foldg runEffect initStore


------------------------------------------------------------------------
-- Simple implementations

newtype Counter = Counter Int deriving (Show,Read,Eq,Ord)

instance Store Counter where
  data Ef Counter = Add Int | Sub Int deriving (Show,Read,Eq,Ord)
  initStore = Counter 0
  defineEffect (Counter n1) e = case e of
                                  Add n2 -> Counter (n1 + n2)
                                  Sub n2 -> Counter (n1 - n2)

instance AStore Counter where
  data Cr Counter = GE | LE deriving (Show,Eq,Ord)
  defineBlock GE e = case e of
                       Add _ -> True
                       _ -> False
  defineBlock LE e = not $ defineBlock GE e
  smartAnd s GE = if Set.member LE s
                     then EQV
                     else Conref $ Set.insert GE s
  smartAnd s LE = if Set.member GE s
                     then EQV
                     else Conref $ Set.insert LE s
