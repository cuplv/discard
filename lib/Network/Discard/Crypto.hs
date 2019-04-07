{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Discard.Crypto 
  ( PK
  , SK
  , FeedId (..)
  , createFeed
  , feedName
  , SCARD (..) 
  , Feed

  ) where

import Crypto.Saltine
import Crypto.Saltine.Class (encode,decode)
import Crypto.Saltine.Core.Hash
import Crypto.Saltine.Core.Sign
import Data.Aeson hiding (encode,decode)
import Data.ByteString (ByteString)
import Data.Text (pack,unpack)

import Data.CARD
import Data.CARD.Store

data PK = PK { pkKey :: PublicKey } deriving (Eq,Ord)

data SK = SK { skKey :: SecretKey } deriving (Eq,Ord)

instance ToJSON PK where
  toJSON (PK pub) = toJSON . pack . show . encode $ pub

instance FromJSON PK where
  parseJSON = withText "PK" (\t -> case decode . read . unpack $ t of
                                     Just pk -> return $ PK pk
                                     Nothing -> fail "No pk.")

instance ToJSON SK where
  toJSON (SK sec) = toJSON . pack . show . encode $ sec

instance FromJSON SK where
  parseJSON = withText "SK" (\t -> case decode . read . unpack $ t of
                                     Just sk -> return $ SK sk
                                     Nothing -> fail "No sk.")

class (CARD d) => SCARD d where
  encodeStoreVal :: d -> ByteString
  encodeEffect :: Ef d -> ByteString

data FeedId d = FeedId PK d

createFeed :: d -> IO (FeedId d,SK)
createFeed d = do (sec,pub) <- newKeypair
                  return (FeedId (PK pub) d, SK sec)

feedName :: (SCARD d) => FeedId d -> ByteString
feedName (FeedId (PK pub) d) = 
  let idEnc = encode pub 
              <> ":" 
              <> encodeStoreVal d
  in hash idEnc

type Feed c d = (FeedId d, Store c PK d)
