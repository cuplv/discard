{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.Discard.Node where

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
import System.Random

import Lang.Carol
import Lang.Carol.Bank
import Data.CvRDT
import Data.CvRDT.Broadcast
import Network.Discard.RepCard
import Data.EventGraph.Ipfs (IpfsEG,mkIpfsEG)

type Script i r s a = 
  i 
  -> ManagerConn i r s
  -> IO a

runNode :: (Ord s, ManC i (IpfsEG i) s () HttpT) 
        => i  -- ^ Name
        -> Int -- ^ IPFS Port
        -> NetConf i -- ^ Replica network
        -> s -- ^ Initial store value
        -> Int -- ^ Timeout unit size (microseconds)
        -> Int
        -> Script i (IpfsEG i) s a -- ^ Actions to perform
        -> IO a
runNode i ipfsPort net s0 n batchSize script = do
  port <- case self i net of
            Just (_,port) -> return port
            Nothing -> die "Given node name is not in network configuration."
  let (otherIds,otherLocs) = unzip (others i net)
  ipfsr <- mkIpfsEG "localhost" ipfsPort i
  httpMan <- mkMan
  otherDests <- mapM (mkDest httpMan) otherLocs
  man <- initManager i otherIds otherDests ipfsr s0 n batchSize
  lt <- mkListener (mkSrc port) man
  res <- script i man
  killThread lt
  killManager man
  return res

mkListener :: (Carries HttpT (CardState i r s))
           => Src HttpT 
           -> ManagerConn i r s
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
