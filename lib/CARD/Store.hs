{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module CARD.Store
  ( Store (..)
  , Ef (..)
  , Cr (..)
  , EGS
  , Summaries
  , Effect
  , ef
  , ef0
  , (|<|)
  , runEffect
  , Conref
  , cr
  , crT
  , crEqv
  , (|&|)
  , impl
  , checkBlock
  , checkBlock'
  , Hist
  , appendAs
  , evalHist
  , evalHistS
  , Counter (..)
  ) where

import Data.Foldable (foldl')
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics
import Data.Aeson

import CARD.EventGraph

class (Eq (Ef s), Eq (Cr s), Ord (Ef s), Ord (Cr s)) => Store s where
  data Ef s
  defineEffect :: s -> Ef s -> s

  data Cr s
  defineConflict :: Cr s -> Ef s -> Bool

class (EG r (i, Effect s) m, Store s) => EGS i r s m
instance (EG r (i, Effect s) m, Store s) => EGS i r s m

type Summaries i r s = Map (Hist i r s) s

-- Note: Internally, an effect is a list of atomic 'Ef' values that is
-- evaluated from *left to right*.  Externally, the composition
-- function '|<|' adds them together such that @e2 |<| e1@ puts the
-- effect of @e1@ before that of @e2@.

newtype Effect s = Effect [Ef s] deriving (Generic)

instance (Ord (Ef s)) => Semigroup (Effect s) where
  (<>) = (|<|)

instance (Ord (Ef s)) => Monoid (Effect s) where
  mempty = ef0

deriving instance (Store s, Read (Ef s)) => Read (Effect s)
deriving instance (Store s, Show (Ef s)) => Show (Effect s)
deriving instance (Store s) => Eq (Effect s)
deriving instance (Store s) => Ord (Effect s)

instance (ToJSON (Ef s)) => ToJSON (Effect s) where
  toEncoding = genericToEncoding defaultOptions

instance (Ord (Ef s), FromJSON (Ef s)) => FromJSON (Effect s)

ef :: Ef s -> Effect s
ef e = Effect [e]

ef0 :: Effect s
ef0 = Effect []

(|<|) :: Effect s -> Effect s -> Effect s
(|<|) (Effect e2) (Effect e1) = Effect (e1 ++ e2)

runEffect :: (Store s) => s -> Effect s -> s
runEffect s (Effect es) = foldl' defineEffect s es

data Conref s = Conref (Set (Cr s)) | EQV deriving (Generic)

instance (Ord (Cr s)) => Semigroup (Conref s) where
  (<>) (Conref s1) (Conref s2) = Conref (s1 <> s2)
  
instance (Ord (Cr s)) => Monoid (Conref s) where
  mempty = crT

deriving instance (Store s) => Eq (Conref s)
deriving instance (Store s) => Ord (Conref s)
deriving instance (Show (Cr s)) => Show (Conref s)

instance (ToJSON (Cr s)) => ToJSON (Conref s) where
  toEncoding = genericToEncoding defaultOptions

instance (Ord (Cr s), FromJSON (Cr s)) => FromJSON (Conref s)

cr :: Cr s -> Conref s
cr c = Conref (Set.singleton c)

crT :: Conref s
crT = Conref (Set.empty)

crEqv :: Conref s
crEqv = EQV

(|&|) :: (Store s) => Conref s -> Conref s -> Conref s
(|&|) (Conref c1) (Conref c2) = Conref (Set.union c1 c2)
(|&|) EQV _ = EQV
(|&|) _ EQV = EQV

impl :: (Store s) => Conref s -> Conref s -> Bool
impl (Conref c1) (Conref c2) = and (Set.map (`Set.member` c1) c2)
impl EQV _ = True
impl (Conref _) EQV = False

-- | Check if conref blocks effect, returning 'True' it does block
checkBlock :: (Store s) => Conref s -> Effect s -> Bool
checkBlock (Conref cs) (Effect es) = or (defineConflict <$> (Set.toList cs) <*> es)
checkBlock EQV (Effect es) = case es of
                               [] -> False
                               _ -> True

-- | Check, returning the 'Left' value if there is a block
checkBlock' :: (Store s) => Conref s -> Effect s -> Either (Conref s) ()
checkBlock' (Conref cs) (Effect es) = 
  if or (defineConflict <$> (Set.toList cs) <*> es)
     then Left (Conref cs)
     else Right ()
checkBlock' EQV (Effect es) = case es of
                                [] -> Right ()
                                _ -> Left EQV

type Hist i r s = Edge r (i, Effect s)

appendAs :: (EG r (i, Effect s) m) => i -> r -> Effect s -> Hist i r s -> m (Hist i r s)
appendAs i r e = append r (i,e)

evalHist :: (Store s, EG r (i, Effect s) m) => r -> s -> Hist i r s -> m s
evalHist res s0 = foldg res (\s (_,e) -> runEffect s e) s0

evalHistS :: (Ord s, Store s, EG r (i, Effect s) m) 
          => r 
          -> s 
          -> Summaries i r s
          -> Hist i r s 
          -> m (s,CacheResult)
evalHistS res s0 = folds res (\(_,e) s -> runEffect s e) s0

------------------------------------------------------------------------
-- Simple implementations

newtype Counter = Counter Int deriving (Show,Read,Eq,Ord,Generic)

instance Store Counter where
  data Ef Counter = Add Int | Sub Int | SetTo Int deriving (Show,Read,Eq,Ord,Generic)
  defineEffect (Counter s) e = case e of
    Add n -> Counter (s + n)
    Sub n -> Counter (s - n)
    SetTo n -> Counter n

  data Cr Counter = LEQ | GEQ deriving (Show,Read,Eq,Ord,Generic)
  defineConflict c e = case (c,e) of
    (LEQ,Sub _) -> True
    (GEQ,Add _) -> True
    (_,SetTo _) -> True
    _ -> False

instance ToJSON (Ef Counter) where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON (Ef Counter)
instance ToJSON (Cr Counter) where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON (Cr Counter)
