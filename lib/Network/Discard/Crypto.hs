{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Discard.Crypto 
  ( PublicKey
  , FeedId (..)
  , createFeed
  , feedName
  , SCARD (..) 

  ) where

import Crypto.Saltine
import Crypto.Saltine.Class (encode)
import Crypto.Saltine.Core.Hash
import Crypto.Saltine.Core.Sign
import Data.ByteString (ByteString)

import Data.CARD

class (CARD d) => SCARD d where
  encodeStoreVal :: d -> ByteString
  encodeEffect :: Ef d -> ByteString

data FeedId d = FeedId PublicKey d

createFeed :: d -> IO (FeedId d,SecretKey)
createFeed d = do (sec,pub) <- newKeypair
                  return (FeedId pub d, sec)

feedName :: (SCARD d) => FeedId d -> ByteString
feedName (FeedId pub d) = 
  let idEnc = encode pub 
              <> ":" 
              <> encodeStoreVal d
  in hash idEnc
