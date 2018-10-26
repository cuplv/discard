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
import Data.ByteString.Lazy (fromStrict,toStrict)
import Data.Text.Encoding (encodeUtf8,decodeUtf8)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Turtle

import Storage.Ipfs.Types
import Storage.Ipfs.Http

import CARD.EventGraph

newtype IpfsEG = IpfsEG IpfsHttp

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
    IpfsEv <$> put api (IpfsObject (decodeUtf8 . toStrict . encode $ d) (nameHist hist))
  unpack (IpfsEG api) (IpfsEv path) = do
    (IpfsObject d links) <- get api path
    case eitherDecode (fromStrict $ encodeUtf8 d) of
      Right dt -> return (dt, Edge . Set.fromList . map IpfsEv . Map.elems $ links)
      Left e -> Turtle.die (Text.pack e)
  vis (IpfsEG api) (IpfsEv p1) (IpfsEv p2) = do
    elem p1 <$> refs api True p2

instance (ToJSON d) => ToJSON (Event IpfsEG d) where
  toJSON (IpfsEv path) = toJSON path

instance (Ord d, FromJSON d) => FromJSON (Event IpfsEG d) where
  parseJSON v = IpfsEv <$> parseJSON v
