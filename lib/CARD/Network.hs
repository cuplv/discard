{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module CARD.Network 
  ( BMsg (..)
  , Transport (..)
  , Dest (..)
  , Src (..)
  , Carries (..)
  , HttpT (..)
  , msgGetter
  , NetConf (..)
  , others
  , self
  , defaultPort

  ) where

import Control.Exception (catch)
import Control.Concurrent.STM
import Data.Aeson
import qualified Network.HTTP.Client as Client
import Network.Wai
import Network.Wai.Handler.Warp
import GHC.Generics
import Network.HTTP.Types
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (foldM)

data BMsg s = BCast s deriving (Generic)

instance (ToJSON s) => ToJSON (BMsg s) where
  toEncoding = genericToEncoding defaultOptions

instance (FromJSON s) => FromJSON (BMsg s)

class Transport t where
  data Dest t
  data Src t
  type Res t :: * -> *

class Carries t s where
  send :: Dest t -> BMsg s -> Res t ()
  listen :: Src t -> (BMsg s -> Res t Bool) -> Res t ()

---

instance Transport (TChan a) where
  data Dest (TChan a) = OutChan (TChan a)
  data Src (TChan a) = InChan (TChan a)
  type Res (TChan a) = STM

instance Carries (TChan (BMsg s)) s where
  send (OutChan chan) msg = writeTChan chan msg
  listen (InChan chan) handle = do
    msg <- readTChan chan
    cont <- handle msg
    if cont
       then listen (InChan chan) handle
       else return ()

data HttpT

instance Transport HttpT where
  data Dest HttpT = HttpDest Client.Manager Client.Request
  data Src HttpT = HttpSrc Port
  type Res HttpT = IO

msgGetter :: (ToJSON (BMsg s), FromJSON (BMsg s)) => (BMsg s -> IO Bool) -> Application
msgGetter handle request respond = do
  body <- strictRequestBody request
  -- putStr "RECV: " >> print body
  case decode body of
    Just msg -> handle msg
  respond $ responseLBS status200 [] "OK"

instance (ToJSON (BMsg s), FromJSON (BMsg s)) => Carries HttpT s where
  send (HttpDest man dest) msg = do
    let req = dest { Client.method = "POST"
                   , Client.requestBody = Client.RequestBodyLBS $ encode msg }
    catch (Client.httpLbs req man >> return ()) (\(Client.HttpExceptionRequest _ _) -> return ())
    -- putStr "SEND: " >> print (encode msg)
    return ()
  listen (HttpSrc p) handle = do
    runSettings (setHost "*" . setPort p $ defaultSettings) (msgGetter handle)

data NetConf i = NetConf (Map i (String, Int)) deriving (Show,Eq,Ord)

others :: (Ord i) => i -> NetConf i -> [(i,(String,Int))]
others i (NetConf m) = filter (\(i',_) -> i /= i') (Map.toList m)

self i (NetConf m) = Map.lookup i m

instance (ToJSON i) => ToJSON (NetConf i) where
  toJSON (NetConf m) = 
    toJSON (map ent $ Map.assocs m)
    where ent (i,(h,p)) = object ["name" .= i
                                 ,"host" .= h
                                 ,"port" .= p]

instance (Ord i, FromJSON i) => FromJSON (NetConf i) where
  parseJSON = withArray "NodeList" $ \v -> NetConf <$> do
    foldM unpack Map.empty v
    where unpack m = withObject "Node" $ \v -> do
            name <- v .: "name"
            host <- v .:? "host" .!= "localhost"
            port <- v .:? "port" .!= defaultPort
            return (Map.insert name (host,port) m)

defaultPort = 23001 :: Int
