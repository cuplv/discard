{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}

module CARD.EventGraph.Ipfs where

import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Turtle

import Storage.Ipfs.Types
import Storage.Ipfs.Turtle

import CARD.EventGraph

newtype IpfsEG = IpfsEG IpfsApi deriving (Eq,Ord)

instance EGB IpfsEG where
  data Event IpfsEG d = IpfsEv IpfsPath deriving (Eq,Ord)

nameHist :: Edge IpfsEG d -> Map FilePath IpfsPath
nameHist = 
  Map.fromList
  . zip (map (\i -> "pre" <> (show $ i)) [0..])
  . map (\(IpfsEv p) -> p)
  . Set.toList
  . edgeSet

instance (Ord d, ToJSON d, FromJSON d) => EG IpfsEG d IO where
  event (IpfsEG api) hist d = do 
    res <- withApi api (putObject (IpfsObject d (nameHist hist)))
    case res of
      Right p -> return $ IpfsEv p
  unpack (IpfsEG api) (IpfsEv path) = do
    res <- withApi api (getObject path)
    case res of
      Right (IpfsObject d links) -> 
        return (d, Edge . Set.fromList . map IpfsEv . Map.elems $ links)
  vis (IpfsEG api) (IpfsEv (IpfsPath p1)) (IpfsEv (IpfsPath p2)) = do
    res <- withApi api $ ipfst ["refs","--recursive",p2] Turtle.empty
    case res of
      Right t -> return (elem p1 . Text.lines $ t)

instance (ToJSON d) => ToJSON (Event IpfsEG d) where
  toJSON (IpfsEv path) = toJSON path
  
instance (Ord d, FromJSON d) => FromJSON (Event IpfsEG d) where
  parseJSON v = IpfsEv <$> parseJSON v
