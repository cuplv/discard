{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}

module CARD.EventGraph.Ipfs 
  ( IpfsEG (..) 
  , mkIpfsEG
  , mkIpfsEG'
  ) where

import System.Exit (die)
import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.ByteString.Lazy (fromStrict,toStrict)
import Data.Text.Encoding (encodeUtf8,decodeUtf8)

import System.IO (hFlush,stdout)

import Storage.Ipfs.Types
import Storage.Ipfs.Http

import CARD.EventGraph.Internal

newtype IpfsEG = IpfsEG IpfsHttp

instance EGB IpfsEG where
  data Event IpfsEG d = IpfsEv IpfsPath deriving (Eq,Ord)

-- | Creates an IPFS 'EventGraph' resolver using an IPFS daemon
-- reachable at the given hostname and port.
mkIpfsEG :: String -- ^ Hostname (such as "localhost")
         -> Int -- ^ Port
         -> IO IpfsEG
mkIpfsEG hostname port = IpfsEG <$> mkIpfsHttp hostname port

-- | Resolver using the default port (5001) on "localhost"
mkIpfsEG' :: IO IpfsEG
mkIpfsEG' = mkIpfsEG "localhost" 5001

nameHist :: Edge IpfsEG d -> Map FilePath IpfsPath
nameHist g = case g of
  Multi s -> Map.fromList
             . zip (map (\i -> "pre" <> (show $ i)) [0..])
             . map (\(IpfsEv p) -> p)
             . Set.toList
             $ s
  Single _ _ (IpfsEv p) -> Map.singleton "pre0" p


instance (Ord d, ToJSON d, FromJSON d) => EG IpfsEG d IO where
  event (IpfsEG api) hist d = do 
    putStr "Putting event... " >> hFlush stdout
    rval <- IpfsEv <$> put api (IpfsObject (decodeUtf8 . toStrict . encode $ d) (nameHist hist))
    putStrLn "Done" >> hFlush stdout
    return rval
  unpack (IpfsEG api) (IpfsEv path) = do
    putStr "Getting event... " >> hFlush stdout
    (IpfsObject d links) <- get api path
    rval <- case eitherDecode (fromStrict $ encodeUtf8 d) of
              Right dt -> return (dt, Multi . Set.fromList . map IpfsEv . Map.elems $ links)
              Left e -> die e
    putStrLn "Done" >> hFlush stdout
    return rval
  vis (IpfsEG api) (IpfsEv p1) (IpfsEv p2) = do
    putStr "Checking vis... " >> hFlush stdout
    rval <- elem p1 <$> refs api True p2
    putStrLn "Done" >> hFlush stdout
    return rval

instance (ToJSON d) => ToJSON (Event IpfsEG d) where
  toJSON (IpfsEv path) = toJSON path

instance (Ord d, FromJSON d) => FromJSON (Event IpfsEG d) where
  parseJSON v = IpfsEv <$> parseJSON v
