{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Network.Discard.Crypto 
  ( PK
  , pkEncode
  , SK
  , makeKeypair
  , Hash
  , hashEncode
  , hashDecode
  , FeedRoot (..)
  , FeedId
  , createFeed
  , feedId
  , SCARD (..) 
  , Feed

  ) where

import Crypto.Saltine
import qualified Crypto.Saltine.Class as Saltine
import Crypto.Saltine.Core.Hash
import Crypto.Saltine.Core.Sign
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text,pack,unpack)
import Data.Text.Encoding (encodeUtf8,decodeUtf8)

import Data.CARD
import Data.CARD.Store

hexToJSON :: ByteString -> Value
hexToJSON = String . decodeUtf8 . B16.encode

hexFromJSON :: Value -> Parser ByteString
hexFromJSON = withText "HexByteString" $ \t -> 
  case B16.decode . encodeUtf8 $ t of
    (bs,"") -> return bs
    (bs,rem) -> fail (show rem)

data PK = PK { pkKey :: PublicKey } deriving (Eq,Ord)

pkEncode :: PK -> Text
pkEncode (PK pub) = decodeUtf8 . B16.encode . Saltine.encode $ pub

instance ToJSON PK where
  toJSON (PK pk) = hexToJSON (Saltine.encode pk)
instance ToJSONKey PK
instance FromJSON PK where
  parseJSON v = (Saltine.decode <$> hexFromJSON v) >>= \case
    Just pk -> return $ PK pk
    Nothing -> fail "Could not decode public key"
instance FromJSONKey PK

data SK = SK { skKey :: SecretKey } deriving (Eq,Ord)

instance ToJSON SK where
  toJSON (SK sk) = hexToJSON (Saltine.encode sk)
instance ToJSONKey SK
instance FromJSON SK where
  parseJSON v = (Saltine.decode <$> hexFromJSON v) >>= \case
    Just sk -> return $ SK sk
    Nothing -> fail "Could not decode secret key"
instance FromJSONKey SK

data Hash = Hash { hashVal :: ByteString } deriving (Eq,Ord)

instance ToJSON Hash where
  toJSON (Hash bs) = hexToJSON bs
instance ToJSONKey Hash
instance FromJSON Hash where
  parseJSON = fmap Hash . hexFromJSON
instance FromJSONKey Hash

hashEncode :: Hash -> Text
hashEncode (Hash bs) = decodeUtf8 . B16.encode $ bs

hashDecode :: Text -> Maybe Hash
hashDecode t = case B16.decode . encodeUtf8 $ t of
                 (bs,"") -> Just (Hash bs)
                 _ -> Nothing

class (CARD d) => SCARD d where
  encodeStoreVal :: d -> ByteString
  encodeEffect :: Ef d -> ByteString

instance SCARD Counter where
  encodeStoreVal = LBS.toStrict . encode
  encodeEffect = LBS.toStrict . encode

instance (Ord a, ToJSON a) => SCARD (RGArray a) where
  encodeStoreVal = LBS.toStrict . encode
  encodeEffect = LBS.toStrict . encode

data FeedRoot d = FeedRoot PK d

instance (ToJSON d) => ToJSON (FeedRoot d) where
  toJSON (FeedRoot pk d) = 
    object ["pubKey" .= pk, "initVal" .= d]

instance (FromJSON d) => FromJSON (FeedRoot d) where
  parseJSON = withObject "FeedRoot" $ \v -> FeedRoot
    <$> v .: "pubKey"
    <*> v .: "initVal"

makeKeypair :: IO (SK,PK)
makeKeypair = do (sec,pub) <- newKeypair
                 return (SK sec, PK pub)

createFeed :: d -> IO (FeedRoot d,SK)
createFeed d = do (sec,pub) <- makeKeypair
                  return (FeedRoot pub d, sec)

type FeedId = Hash


feedId :: (SCARD d) => FeedRoot d -> FeedId
feedId (FeedRoot (PK pub) d) = 
  let idEnc = Saltine.encode pub 
              <> ":" 
              <> encodeStoreVal d
  in Hash $ hash idEnc

type Feed c d = (FeedRoot d, Store c PK d)
