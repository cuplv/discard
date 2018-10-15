{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module CARD.Store where

import Data.Foldable (foldl')
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics
import Data.Aeson

import CARD.EventGraph

class (Eq (Ef s), Eq (Cr s), Ord (Ef s), Ord (Cr s)) => Store s where
  data Ef s
  defineEffect :: s -> Ef s -> s

  data Cr s
  defineConflict :: Cr s -> Ef s -> Bool
  
newtype Effect s = Effect [Ef s] deriving (Generic)

deriving instance (Store s, Read (Ef s)) => Read (Effect s)
deriving instance (Store s, Show (Ef s)) => Show (Effect s)
deriving instance (Store s) => Eq (Effect s)
deriving instance (Store s) => Ord (Effect s)

instance (ToJSON (Ef s)) => ToJSON (Effect s) where
  toEncoding = genericToEncoding defaultOptions

instance (Ord (Ef s), FromJSON (Ef s)) => FromJSON (Effect s)

runEffect :: (Store s) => s -> Effect s -> s
runEffect s (Effect es) = foldl' defineEffect s es

data Conref s = Conref (Set (Cr s)) | EQV deriving (Generic)

deriving instance (Store s) => Eq (Conref s)
deriving instance (Store s) => Ord (Conref s)

instance (ToJSON (Cr s)) => ToJSON (Conref s) where
  toEncoding = genericToEncoding defaultOptions

instance (Ord (Cr s), FromJSON (Cr s)) => FromJSON (Conref s)

checkBlock :: (Store s) => Conref s -> Effect s -> Bool
checkBlock (Conref cs) (Effect es) = or (defineConflict <$> (Set.toList cs) <*> es)
checkBlock EQV (Effect es) = case es of
                               [] -> False
                               _ -> True

-- evalHistory :: (MonadEG g (Effect s) m, Store s) => g (Effect s) -> m s
-- evalHistory = foldg runEffect initStore


------------------------------------------------------------------------
-- Simple implementations

newtype Counter = Counter Int deriving (Show,Read,Eq,Ord,Generic)

instance Store Counter where
  data Ef Counter = Add Int | Sub Int | SetTo Int deriving (Show,Read,Eq,Ord,Generic)
  defineEffect (Counter s) e = case e of
    Add n -> Counter (s + n)
    Sub n -> Counter (s - n)
    SetTo n -> Counter n

  data Cr Counter = TODO deriving (Show,Read,Eq,Ord,Generic)
  defineConflict = undefined

instance ToJSON (Ef Counter) where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON (Ef Counter)
instance ToJSON (Cr Counter) where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON (Cr Counter)

-- instance CARD Counter where
--   data Ef Counter = Add Int | Sub Int deriving (Show,Read,Eq,Ord)
--   initStore = Counter 0
--   defineEffect (Counter n1) e = case e of
--                                   Add n2 -> Counter (n1 + n2)
--                                   Sub n2 -> Counter (n1 - n2)

-- instance CARD Counter where
--   data Cr Counter = GE | LE deriving (Show,Eq,Ord)
--   defineBlock GE e = case e of
--                        Add _ -> True
--                        _ -> False
--   defineBlock LE e = not $ defineBlock GE e
--   smartAnd s GE = if Set.member LE s
--                      then EQV
--                      else Conref $ Set.insert GE s
--   smartAnd s LE = if Set.member GE s
--                      then EQV
--                      else Conref $ Set.insert LE s
