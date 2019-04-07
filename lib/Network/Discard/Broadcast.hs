{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Network.Discard.Broadcast 
  ( BMsg (..)
  -- , msgGetter
  -- , NetConf (..)
  -- , others
  -- , self
  -- , defaultPort
  -- , broadcast
  -- , helloAll

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
import Data.Maybe (catMaybes)
import System.IO (hFlush,stdout)
import qualified Data.ByteString.Lazy.Char8 as BS

-- -- | 'BCast' constructs a broadcast for whatever data is being
-- -- carried, and 'Hello' constructs a simple "I'm new here"
-- -- announcement, which should be responded to with a 'BCast' update.

-- | A message from the network, along with callbacks for responding.
--
-- 'BCast' represents a state broadcast received from the network.
-- The @Bool -> IO ()@ function included is used to acknowledge.  If
-- the included state includes new information, ack with 'True'.  If
-- not, ack with 'False'.
--
-- 'SReq' represents a request for the current state.  It may have
-- been sent by a replica that has just started up.  The included @s
-- -> IO ()@ function is used to send a state directly to the
-- requesting replica (not a broadcast).
data BMsg s = BCast s (Bool -> IO ()) | SReq (s -> IO ()) deriving (Generic)

-- class Transport t where
--   data Dest t
--   data Src t
--   type Res t :: * -> *

-- class Carries t s where
--   send :: Dest t -> BMsg s -> Res t ()

--   -- | Asdf
--   listen :: Int -> Src t -> (BMsg s -> Res t (Bool, Maybe (BMsg s))) -> Res t ()
--   -- | Send a 'Hello' message
--   hello :: Dest t -> Res t (Maybe (BMsg s))

-- instance Transport (TChan (BMsg s)) where
--   data Dest (TChan (BMsg s)) = OutChan (TChan (BMsg s))
--   data Src (TChan (BMsg s)) = InChan (TChan (BMsg s))
--   type Res (TChan (BMsg s)) = STM

-- instance Carries (TChan (BMsg s)) s where
--   hello (OutChan chan) = writeTChan chan Hello >> return Nothing
--   send (OutChan chan) msg = writeTChan chan msg
--   listen dbg (InChan chan) handle = do
--     msg <- readTChan chan
--     (cont,mresp) <- handle msg
--     if cont
--        then listen dbg (InChan chan) handle
--        else return ()

-- data HttpT

-- instance Transport HttpT where
--   data Dest HttpT = HttpDest Client.Manager Client.Request
--   data Src HttpT = HttpSrc Port
--   type Res HttpT = IO

-- dbg d t s = if t <= d
--                then putStrLn s >> hFlush stdout
--                else return ()

