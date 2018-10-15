{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module CARD.Network where

import Control.Concurrent.STM
import Data.Aeson
import qualified Network.HTTP.Client as Client
import Network.Wai
import Network.Wai.Handler.Warp
import GHC.Generics
import Network.HTTP.Types

import CARD.Store
import CARD.EventGraph

data CMsg i r s = BCast i (Edge r (i, Effect s))
                | ARequest i (Conref s)
                | AGrant i (Conref s)
                | ARelease i
                deriving (Generic)

instance (ToJSON i, ToJSON (Cr s), ToJSON (Event r (i, Effect s))) => ToJSON (CMsg i r s) where
  toEncoding = genericToEncoding defaultOptions

instance (FromJSON i, Ord (Cr s), FromJSON (Cr s), Ord (Event r (i, Effect s)), FromJSON (Event r (i, Effect s))) => FromJSON (CMsg i r s)

class Transport t where
  data Dest t
  data Src t
  type Res t :: * -> *

class Carries t i r s where
  send :: Dest t -> CMsg i r s -> Res t ()
  listen :: Src t -> (CMsg i r s -> Res t Bool) -> Res t ()

---

instance Transport (TChan a) where
  data Dest (TChan a) = OutChan (TChan a)
  data Src (TChan a) = InChan (TChan a)
  type Res (TChan a) = STM

instance Carries (TChan (CMsg i r s)) i r s where
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

msgGetter :: (ToJSON (CMsg i r s), FromJSON (CMsg i r s)) => (CMsg i r s -> IO Bool) -> Application
msgGetter handle request respond = do
  putStrLn "Msg received."
  body <- strictRequestBody request
  case decode body of
    Just msg -> handle msg
  respond $ responseLBS status200 [] "Thanks."

instance (ToJSON (CMsg i r s), FromJSON (CMsg i r s)) => Carries HttpT i r s where
  send (HttpDest man dest) msg = do
    let req = dest { Client.method = "POST"
                   , Client.requestBody = Client.RequestBodyLBS $ encode msg }
    response <- Client.httpLbs req man
    return ()
  listen (HttpSrc p) handle = do
    run p (msgGetter handle)
