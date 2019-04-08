{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Discard.HTTP
  ( mkPhone
  , msgListener
  , requestState
  , sendState
  , DeliverStatus (..)
  , Wai.Application

  ) where

import Control.Concurrent.STM
import Control.Exception (catch)
import Data.Aeson
import Data.Maybe
import GHC.Generics
import qualified Network.HTTP.Client as Client
import Network.HTTP.Types
import qualified Network.Wai as Wai

import Network.Discard.Broadcast
import Network.Discard.Crypto (PK,FeedId)

mkPhone :: (ToJSON s, FromJSON s)
        => s -- ^ Dummy feed value to set type
        -> FeedId
        -> [String] -- ^ List of remote addresses for broadcast
        -> IO (Phone s)
mkPhone s00 fid ds = do
  man <- Client.newManager Client.defaultManagerSettings
  let send s = anyReceived <$> mapM (sendState man fid s) ds
      ask = catMaybes <$> mapM (requestState s00 man fid) ds
  return $ Phone send ask


data WireMsg s = WBCast s | WSReq deriving (Generic)

instance (ToJSON s) => ToJSON (WireMsg s) where
  toEncoding = genericToEncoding defaultOptions

instance (FromJSON s) => FromJSON (WireMsg s)

-- | A WAI application that handles broadcast network messages.
--
-- The "handler action" is called for each received state broadcast or
-- state request.  The "rebroadcast action" is passed on to the
-- handler, which should call it if the received state contains new
-- information (and thus should be gossip'ed along).
msgListener :: (ToJSON s, FromJSON s) 
            => (FeedId -> BMsg s -> IO ()) -- ^ Handler action
            -> (FeedId -> s -> IO ()) -- ^ Rebroadcast action
            -> Wai.Application
msgListener handle rebc request respond = do
  body <- Wai.strictRequestBody request
  case decode body of
    Just (i, WBCast f) -> do 
      let ack b = if b
                     then rebc i f
                     else return ()
      handle i (BCast f ack)
      respond $ Wai.responseLBS status200 [] ""
    Just (i, WSReq) -> do
      var <- newEmptyTMVarIO
      let job = SReq (atomically . putTMVar var)
      handle i job
      s <- atomically $ takeTMVar var
      respond $ Wai.responseLBS status200 [] (encode s)

-- | Request the latest head of a feed by its ID.  The first @s@
-- parameter simply decides the type, and is not used.
requestState :: (ToJSON s, FromJSON s) => s -> Client.Manager -> FeedId -> String -> IO (Maybe s)
requestState (_::s) man i uri = do
  reqInit <- Client.parseRequest uri
  let req = reqInit { Client.requestBody = 
                        Client.RequestBodyLBS $ encode (i, WSReq::WireMsg s) }
      sendRcv = do
        resp <- Client.httpLbs req man
        print (Client.responseBody resp)
        return $ decode' (Client.responseBody resp)
  catch sendRcv (\(Client.HttpExceptionRequest _ _) -> return Nothing)

-- | Acked: the receiver has accepted and incorporated the update.
-- Received: the message was delivered without error (but no info on
-- whether it was accepted).  Failed: the message was not delivered,
-- possibly because the target could not be reached.
data DeliverStatus = Acked | Received | Failed deriving (Show,Read,Eq,Ord)

anyReceived :: [DeliverStatus] -> Bool
anyReceived ds = length (filter (/= Failed) ds) > 0

-- | Send the current head of a feed.
sendState :: (ToJSON s) => Client.Manager -> FeedId -> s -> String -> IO DeliverStatus
sendState man i s uri = do
  reqInit <- Client.parseRequest uri
  let req = reqInit { Client.requestBody = 
                        Client.RequestBodyLBS $ encode (i, WBCast s) }
  catch (Client.httpLbs req man >> return Received)
        (\(Client.HttpExceptionRequest _ _) -> return Failed)
