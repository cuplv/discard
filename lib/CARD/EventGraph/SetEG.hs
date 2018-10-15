{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}

module CARD.EventGraph.SetEG where

import Control.Monad.Identity
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Data.Foldable (foldl')
import Data.Aeson

import CARD.EventGraph

data SetEG = SetEG deriving (Eq, Ord)

instance EGB SetEG where
  data Event SetEG d = (Edge SetEG d) :<: d

deriving instance (Eq d) => Eq (Event SetEG d)
deriving instance (Ord d) => Ord (Event SetEG d)
deriving instance (Show d) => Show (Event SetEG d)

instance (Monad m, Ord d) => EG SetEG d m where
  event _ g d = return $ g :<: d
  unpack _ (g :<: d) = return (d,g)
  vis r e1 (es :<: _) = 
    if Set.member e1 (edgeSet es)
       then return True
       else or <$> mapM (vis r e1) (Set.toList . edgeSet $ es)

instance (ToJSON d) => ToJSON (Event SetEG d) where
  toJSON (e :<: d) = object ["payload" .= d, "history" .= e]

instance (Ord d, FromJSON d) => FromJSON (Event SetEG d) where
  parseJSON = withObject "SetEG Event" $ \v -> (:<:)
    <$> v .: "history"
    <*> v .: "payload"
