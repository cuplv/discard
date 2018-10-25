{-# LANGUAGE OverloadedStrings #-}

-- | IPFS bindings via the go-ipfs daemon's HTTP API
module Storage.Ipfs.Http 
  ( IpfsHttp
  , mkIpfsHttp
  , put
  , get
  ) where

import qualified Data.Text as Text
import Network.HTTP.Types
import Network.HTTP.Client
import Network.HTTP.Client.MultipartFormData
import Data.ByteString
import Data.ByteString.Lazy as LZ
import Data.Aeson

import Storage.Ipfs.Types

data IpfsHttp = IpfsHttp Manager String

-- | Create an IPFS HTTP API endpoint connection
mkIpfsHttp :: String -- ^ Hostname or IP address
           -> String -- ^ Port
           -> IO IpfsHttp
mkIpfsHttp addr port = IpfsHttp
  <$> newManager defaultManagerSettings
  <*> pure ("http://"++addr++ ":"++port)

newtype PutResponse = PutResponse { putResponse :: IpfsPath }

instance FromJSON PutResponse where
  parseJSON = withObject "PutResponse" $ \v -> do
    hash <- v .: "Hash"
    return (PutResponse $ IpfsPath hash)

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
  resp <- decode . responseBody <$> httpLbs fullReq man
  case resp of
    Just (PutResponse path) -> return path

-- | Get an IPFS object
get :: IpfsHttp -> IpfsPath -> IO IpfsObject
get (IpfsHttp man host) (IpfsPath hash) = do
  fullReq <- parseRequest (host++"/api/v0/object/get?arg="++Text.unpack hash)
  resp <- decode . responseBody <$> httpLbs fullReq man
  case resp of
    Just obj -> return obj

