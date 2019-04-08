{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Network.Discard.Crypto 
  ( PK
  , SK
  , Hash
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
import Data.Text (pack,unpack)
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

instance ToJSON PK where
  toJSON (PK pk) = hexToJSON (Saltine.encode pk)
instance FromJSON PK where
  parseJSON v = (Saltine.decode <$> hexFromJSON v) >>= \case
    Just pk -> return $ PK pk
    Nothing -> fail "Could not decode public key"

data SK = SK { skKey :: SecretKey } deriving (Eq,Ord)

instance ToJSON SK where
  toJSON (SK sk) = hexToJSON (Saltine.encode sk)
instance FromJSON SK where
  parseJSON v = (Saltine.decode <$> hexFromJSON v) >>= \case
    Just sk -> return $ SK sk
    Nothing -> fail "Could not decode secret key"

data Hash = Hash { hashVal :: ByteString } deriving (Eq,Ord)

instance ToJSON Hash where
  toJSON (Hash bs) = hexToJSON bs
instance FromJSON Hash where
  parseJSON = fmap Hash . hexFromJSON

class (CARD d) => SCARD d where
  data Decl d
  encodeStoreVal :: d -> ByteString
  encodeEffect :: Ef d -> ByteString

data FeedRoot d = FeedRoot PK d

createFeed :: d -> IO (FeedRoot d,SK)
createFeed d = do (sec,pub) <- newKeypair
                  return (FeedRoot (PK pub) d, SK sec)

type FeedId = Hash


feedId :: (SCARD d) => FeedRoot d -> FeedId
feedId (FeedRoot (PK pub) d) = 
  let idEnc = Saltine.encode pub 
              <> ":" 
              <> encodeStoreVal d
  in Hash $ hash idEnc

type Feed c d = (FeedRoot d, Store c PK d)
