{-# LANGUAGE OverloadedStrings #-}

-- | IPFS bindings via the go-ipfs daemon's HTTP API
module Storage.Ipfs.Http 
  ( IpfsHttp
  , mkIpfsHttp
  , put
  , get
  , refs
  ) where

import qualified Data.Text as Text
import Network.HTTP.Types
import Network.HTTP.Client
import Network.HTTP.Client.MultipartFormData
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C
import Data.Aeson
import Data.Aeson.Parser (decodeWith)
import Data.Aeson.Types (parse)
import Control.Monad

import Storage.Ipfs.Types

parse' p b = decodeWith json (parse p) b

data IpfsHttp = IpfsHttp Manager String

-- | Create an IPFS HTTP API endpoint connection
mkIpfsHttp :: String -- ^ Hostname or IP address
           -> String -- ^ Port
           -> IO IpfsHttp
mkIpfsHttp addr port = IpfsHttp
  <$> newManager defaultManagerSettings
  <*> pure ("http://"++addr++ ":"++port)

putRespP = withObject "PutResponse" $ \v -> do
  hash <- v .: "Hash"
  return (IpfsPath hash)

-- | Put an IPFS object, returning the ref created for it
--
-- Example:
-- 
-- > putBaz :: IO IpfsPath
-- > putBaz = do api <- mkIpfsHttp "localhost" "5001"
-- >             foo <- put api (objData "foo")
-- >             bar <- put api (objData "bar")
-- >             put api (objData "baz" <> objLink "l1" foo <> objLink "l2" bar)
put :: IpfsHttp -> IpfsObject -> IO IpfsPath
put (IpfsHttp man host) obj = do
  initReq <- parseRequest (host++ "/api/v0/object/put")
  fullReq <- formDataBody [partLBS "file" (encode obj)] initReq
  resp <- (parse' putRespP) . responseBody <$> httpLbs fullReq man
  case resp of
    Just path -> return path

-- | Get an IPFS object
get :: IpfsHttp -> IpfsPath -> IO IpfsObject
get (IpfsHttp man host) (IpfsPath hash) = do
  fullReq <- parseRequest (host++"/api/v0/object/get?arg="++Text.unpack hash)
  resp <- decode . responseBody <$> httpLbs fullReq man
  case resp of
    Just obj -> return obj

putBaz :: IpfsHttp -> IO IpfsPath
putBaz api = do foo <- put api (objData "foo")
                bar <- put api (objData "bar")
                put api (objData "baz" <> objLink "l1" foo <> objLink "l2" bar)

testy = do api <- mkIpfsHttp "localhost" "5001"
           baz <- putBaz api
           quux <- put api (objData "quux" <> objLink "baznode" baz)
           Prelude.putStrLn "Refs of baz:"
           mapM_ print =<< refs api False baz
           Prelude.putStrLn "R-refs of baz:"
           mapM_ print =<< refs api True baz
           Prelude.putStrLn "Refs of quux:"
           mapM_ print =<< refs api False quux
           Prelude.putStrLn "R-Refs of quux:"
           mapM_ print =<< refs api True quux

refsRespP = withObject "RefsResponse" $ \v -> do
  hash <- v .: "Ref"
  return (IpfsPath hash)

-- | List the links (direct or recursive) from the object pointed
-- to by the supplied ref
refs :: IpfsHttp 
     -> Bool -- ^ Recursive?
     -> IpfsPath -- ^ Root
     -> IO [IpfsPath]
refs (IpfsHttp man host) recursive (IpfsPath hash) = do
  let rb = if recursive
              then "true"
              else "false"
  fullReq <- parseRequest (host ++ "/api/v0/refs?arg="++Text.unpack hash ++ "&recursive=" ++rb)
  rs <- map BL.fromStrict . C.lines . BL.toStrict . responseBody <$> httpLbs fullReq man
  let p vs b = case (parse' refsRespP b) of
                 Just v -> Just (v:vs)
                 Nothing -> Nothing
  case foldM p [] rs of
    Just vs -> return vs
