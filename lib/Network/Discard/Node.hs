{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.Discard.Node
  ( Script
  , runCarolR
  , runCarolM
  , ManagerConn
  , runNode
  ) where

import System.IO
import System.Exit
import Data.List (genericLength)
import Control.Monad
import Control.Monad.Trans (MonadIO,liftIO)
import Control.Concurrent (forkIO,ThreadId,threadDelay,killThread)
import Control.Concurrent.STM hiding (check)
import Network.HTTP.Client
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Yaml
import Data.Aeson.Types (ToJSONKey, FromJSONKey)
import System.Random

import Data.CvRDT
import Data.CARD.Store
import Data.EventGraph (Edge)

import Network.Discard.Broadcast
import Network.Discard.RepCard
import Data.EventGraph.Ipfs (IpfsEG,mkIpfsEG)

-- | A program to run on a replica node, which has access to the
-- state-management thread via the 'ManagerConn' in order to execute
-- non-trivial operations.
type Script r c i s a = 
  i 
  -> ManagerConn c i s
  -> IO a

-- | Run a complete replica node, establishing network and state
-- management threads, and execute a replica script on it.
--
-- If/when the script terminates, the replica node will be torn down
-- and background threads killed.
runNode :: (Ord s, ManC (IpfsEG i) c i s (), ToJSON (c (i, Effect s)), ToJSON i, ToJSONKey i, ToJSON (Cr s), ToJSON (Ef s), FromJSONKey i, FromJSON i, FromJSON (Cr s), FromJSON (Ef s), FromJSON (c (i, Effect s)), c ~ Edge (IpfsEG i))
        => i  -- ^ Name
        -> Int -- ^ IPFS Port
        -> NetConf i -- ^ Replica network
        -> s -- ^ Initial store value
        -> Hist c i s -- ^ Initial history
        -> s -- ^ Base store value
        -> Int -- ^ Timeout unit size (microseconds)
        -> Int -- ^ Batch size
        -> (s -> IO ()) -- ^ Callback for store updates
        -> Script (IpfsEG i) c i s a -- ^ Actions to perform
        -> IO (a, s, Hist c i s)
runNode i ipfsPort net s0 hist0 s00 n batchSize cb script = do
  port <- case self i net of
            Just (_,port) -> return port
            Nothing -> die "Given node name is not in network configuration."
  let (otherIds,otherLocs) = unzip (others i net)
  ipfsr <- mkIpfsEG "localhost" ipfsPort i
  httpMan <- mkMan
  otherDests <- mapM (mkDest httpMan) otherLocs
  man <- initManager i otherIds otherDests ipfsr s0 hist0 s00 n batchSize cb
  lt <- mkListener (mkSrc port) man
  res <- script i man
  killThread lt
  (sf,hf) <- killManager man
  return (res, sf, hf)

mkListener :: (Carries HttpT (Store c i s))
           => Src HttpT 
           -> ManagerConn c i s
           -> IO ThreadId
mkListener src man = 
  forkIO (listen src (\m -> giveUpdate m man 
                            >> return True))

mkMan :: IO Manager
mkMan = newManager defaultManagerSettings

mkDest :: Manager -> (String,Int) -> IO (Dest HttpT)
mkDest man (host,port) = HttpDest 
  <$> pure man 
  <*> parseRequest ("http://" ++ host ++ ":" ++ show port)

mkSrc :: Int -> Src HttpT
mkSrc = HttpSrc
