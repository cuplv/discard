{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module CARD.Node where

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

import CARD
import CARD.LQ.Bank
import CARD.EventGraph.Ipfs (IpfsEG,mkIpfsEG')

type Script i a = 
     Int 
  -> i 
  -> TQueue (Either (BMsg (CardState i (IpfsEG i) Counter)) (Job Counter)) 
  -> TMVar Bool
  -> TVar Counter
  -> IO a

runNode :: String
        -> NetConf String
        -> Script String a
        -> IO a
runNode i net script = do
  port <- case self i net of
            Just (_,port) -> return port
            Nothing -> die "Given node name is not in network configuration."
  let (otherIds,otherLocs) = unzip (others i net)
  ipfsr <- mkIpfsEG' i
  httpMan <- mkMan
  otherDests <- mapM (mkDest httpMan) otherLocs
  (inbox,result,latest) <- initManager i otherIds otherDests ipfsr (Counter 100000)
  lt <- mkListener (mkSrc port) inbox
  res <- script 0 i inbox result latest
  killThread lt
  return res

mkListener :: (Carries HttpT (CardState i r s))
           => Src HttpT 
           -> TQueue (Either (BMsg (CardState i r s)) j) 
           -> IO ThreadId
mkListener src q = 
  forkIO (listen src (\m -> atomically (writeTQueue q (Left m)) 
                            >> return True))

mkMan :: IO Manager
mkMan = newManager defaultManagerSettings

mkDest :: Manager -> (String,Int) -> IO (Dest HttpT)
mkDest man (host,port) = HttpDest 
  <$> pure man 
  <*> parseRequest ("http://" ++ host ++ ":" ++ show port)

mkSrc :: Int -> Src HttpT
mkSrc = HttpSrc
