{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.IO
import System.Directory
import System.Exit
import Data.List (genericLength)
import Control.Monad
import Control.Monad.Trans (MonadIO,liftIO)
import Control.Concurrent (forkIO,ThreadId,threadDelay)
import Control.Concurrent.STM hiding (check)
import Network.HTTP.Client
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Yaml
import Options.Applicative
import System.Random
import Data.Time.Clock

import Lang.Carol
import Lang.Carol.Bank
import Network.Discard
import Data.EventGraph
import Data.CARD.Store
import Data.EventGraph.Ipfs (IpfsEG)

import Interface

main :: IO ()
main = node
-- main = runUi

data ConfCLI = ConfCLI
  { confFile :: FilePath
  , nodeName :: String
  , ipfsPort :: Int
  , pFile :: Maybe FilePath }

optionm p os = (Just <$> option p os) <|> pure Nothing

confCLI :: IO ConfCLI
confCLI = execParser $ 
  let parser = ConfCLI
        <$> strOption (short 'c' 
                       <> metavar "FILE" 
                       <> help "Network configuration file")
        <*> strOption (short 'i' 
                       <> metavar "NAME" 
                       <> help "Node name")
        <*> option auto (long "ipfs-port" <> value 5001)
        <*> optionm str (short 'f' <> metavar "FILE" <> help "Persistent storage file")
      misc = (fullDesc
              <> progDesc "Run a bank account CARD node"
              <> header "discard-demo - a demo application for the Carol language")
  in info (parser <**> helper) misc

tryPFile :: FilePath -> IO (Maybe (Counter, Hist (Edge (IpfsEG String)) String Counter))
tryPFile fp = doesFileExist fp >>= \case
  True -> decodeFileEither fp >>= \case
    Right state -> return (Just state)
    Left e -> do print e
                 die $ "Save file \"" <> fp <> "\" exists but is unreadable."
  False -> return Nothing

emptyInit = (Counter 0, Data.EventGraph.empty)


node :: IO ()
node = do
  conf <- confCLI
  net <- decodeFileEither (confFile conf) >>= \case
    Right net -> return net
    Left exc -> print exc >> die "Could not read network configuration file"
  (initStore,initHist) <- case pFile conf of
                            Just fp -> tryPFile fp >>= \case
                                         Just r -> return r
                                         _ -> return emptyInit
                            _ -> return emptyInit
  
  (eventChan, onUpdate, onMessage) <- mkUpdateChan

  let script2 i man = runUi initStore man eventChan
      settings = defaultDManagerSettings { onStoreUpdate = onUpdate . fst
                                         , onGetBroadcast = onMessage }

  (_, sf, hf) <- runNode (nodeName conf) (ipfsPort conf) net initStore initHist settings script2
  case pFile conf of
    Just fp -> encodeFile fp (sf,hf)
    _ -> return ()
