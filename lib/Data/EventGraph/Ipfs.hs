{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.EventGraph.Ipfs 
  ( IpfsEG
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
import GHC.Generics

import System.IO (hFlush,stdout)

import Storage.Ipfs.Types
import Storage.Ipfs.Http

import Data.EventGraph.Internal
import Data.LamportClock


data IpfsEG i = IpfsEG IpfsHttp i

instance (Ord i) => EGB (IpfsEG i) where
  data Event (IpfsEG i) d = IpfsEv IpfsPath (i,LClock i) deriving (Eq,Ord,Generic)

nameHistL :: Edge (IpfsEG i) d -> Map FilePath (IpfsPath,(i,LClock i))
nameHistL g = case g of
  Multi s -> Map.fromList
             . zip (map (\i -> "pre" <> (show $ i)) [0..])
             . map (\(IpfsEv p lc) -> (p,lc))
             . Set.toList
             $ s
  Single _ _ (IpfsEv p cl) -> Map.singleton "pre0" (p,cl)

instance (Ord i, ToJSON i, ToJSONKey i, FromJSON i, FromJSONKey i, Ord d, ToJSON d, FromJSON d) => EG (IpfsEG i) d IO where

  event (IpfsEG api i) hist d = do 
    putStr "Putting event... " >> hFlush stdout
    let histMap = nameHistL hist
        histLinks = fmap fst histMap
        histClocks = fmap snd histMap
    ref <- put api (IpfsObject (decodeUtf8 . toStrict . encode $ (d,histClocks)) histLinks)
    putStrLn "Done" >> hFlush stdout
    let clock = foldr (\ce ca -> uncurry tick ce <> ca) mempty histClocks
    return (IpfsEv ref (i,clock))

  unpack (IpfsEG api i) (IpfsEv path (ie,cl)) = do
    putStr "Getting event... " >> hFlush stdout
    (IpfsObject dt links) <- get api path
    (d,histClocks) <- case eitherDecode (fromStrict $ encodeUtf8 dt) of
                           Right d -> return d
                           Left e -> die e
    putStrLn "Done" >> hFlush stdout
    let hist = Map.intersectionWith IpfsEv links histClocks
    return (d, Multi . Set.fromList . Map.elems $ hist)

  vis (IpfsEG api i) (IpfsEv p1 (i1,cl1)) (IpfsEv p2 (i2,cl2)) =
    return $ upto i1 (tick i1 cl1) <= upto i1 cl2

instance (ToJSON d, ToJSON i, ToJSONKey i) => ToJSON (Event (IpfsEG i) d) where
  toEncoding = genericToEncoding defaultOptions

instance (Ord i, FromJSON i, FromJSONKey i, Ord d, FromJSON d) => FromJSON (Event (IpfsEG i) d) where

-- | Creates an IPFS 'EventGraph' resolver using an IPFS daemon
-- reachable at the given hostname and port.
mkIpfsEG :: String -- ^ Hostname (such as "localhost")
             -> Int -- ^ Port
             -> i -- ^ Replica ID
             -> IO (IpfsEG i)
mkIpfsEG hostname port i = IpfsEG <$> mkIpfsHttp hostname port <*> pure i

-- | Resolver using the default port (5001) on "localhost"
mkIpfsEG' :: i -> IO (IpfsEG i)
mkIpfsEG' = mkIpfsEG "localhost" 5001
