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
import qualified Network.Wai.Handler.Warp as Warp
import Data.Map (Map)
import Data.Text (Text,unpack)
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
import Data.EventGraph (empty)
import Data.EventGraph.Ipfs (mkIpfsEG,IpfsEG)

import Interface

main :: IO ()
main = node

data ConfCLI = ConfCLI
  { listenPort :: Int
  , ipfsPort :: Int
  , remoteAddrs :: [String]
  , pFile :: Maybe FilePath
  , followFeed :: Maybe Text }

-- data ConfigFile = undefined

localAddr :: ConfCLI -> String
localAddr c = "http://localhost:" <> show (ipfsPort c)

optionm p os = (Just <$> option p os) <|> pure Nothing

confCLI :: IO ConfCLI
confCLI = execParser $ 
  let parser = ConfCLI
        <$> argument auto (metavar "PORT" <> help "Port to listen on")
        <*> option auto (long "ipfs" <> value 5001 <> metavar "IPFS_PORT")
        <*> option auto (long "remotes" <> value [] <> metavar "[URI]")
        <*> optionm str (short 'f' <> metavar "FILE" <> help "Persistent storage file")
        <*> optionm str (long "feed" <> metavar "ID")
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
  (_,i) <- makeKeypair
  (eventChan, onUpdate, onMessage) <- mkUpdateChan
  let sets = defaultDManagerSettings { onValUpdate = onUpdate
                                     , onGetBroadcast = onMessage}
  (fid,conn) <- case hashDecode <$> followFeed conf of
                  Just (Just f) -> follow i conf sets f >>= \case
                    Just conn -> return (f,conn)
                    Nothing -> die "The requested feed could not be found among the supplied remotes."
                  Just Nothing -> die "The supplied feed ID could not be understood."
                  Nothing -> mkNew i conf sets
  putStrLn "Getting init store"
  initStore <- carol conn queryT
  putStrLn "Running gui"
  runUi (i,fid) initStore conn eventChan

mkPhone' :: FeedId -> [String] -> IO (Phone (Feed (Edge (IpfsEG PK)) Counter))
mkPhone' fid ds = do 
  -- Annoyingly, we need a token feed value to set the type...
  (r,_) <- createFeed (Counter 0)
  let feed00 = (r,(mempty, Data.EventGraph.empty))
  
  mkPhone feed00 fid ds

follow :: PK 
       -> ConfCLI 
       -> DManagerSettings (Edge (IpfsEG PK)) Counter 
       -> FeedId 
       -> IO (Maybe (ManagerConn (Edge (IpfsEG PK)) Counter))
follow i conf sets fid = do
  phone <- mkPhone' fid (remoteAddrs conf)
  res <- mkIpfsEG (localAddr conf) i
  askStates phone >>= \case
    [] -> do putStrLn "Could not find specified feed at any replica."
             return Nothing
    s:ss -> do
      conn <- initManager i [] phone res s sets
      mapM_ (\s -> giveUpdate (BCast s (\_ -> return ())) conn) ss
      runListener (listenPort conf) fid conn
      return (Just conn)

runListener port fid conn = do
  let handle fid' msg = if fid' == fid
                           then giveUpdate msg conn
                           else putStrLn "Got msg for unknown feed."
      rebc = \_ _ -> return ()
      listener = msgListener handle rebc
      settings = Warp.setHost "!6" . Warp.setPort port $ Warp.defaultSettings
  forkIO $ Warp.runSettings settings listener
  return ()

mkNew :: PK -> ConfCLI -> DManagerSettings (Edge (IpfsEG PK)) Counter -> IO (FeedId, ManagerConn (Edge (IpfsEG PK)) Counter)
mkNew i conf sets = do
  (r,_) <- createFeed (Counter 8)
  p <- mkPhone' (feedId r) (remoteAddrs conf)
  res <- mkIpfsEG (localAddr conf) i
  let feed = (r,(mempty, Data.EventGraph.empty))
  conn <- initManager i [] p res feed sets
  runListener (listenPort conf) (feedId r) conn
  return (feedId r, conn)

-- node :: IO ()
-- node = do
--   conf <- confCLI
--   net <- decodeFileEither (confFile conf) >>= \case
--     Right net -> return net
--     Left exc -> print exc >> die "Could not read network configuration file"

--   let runNode' settings script = case pFile conf of
--         Just sfile -> 
--           runNodeFile (nodeName conf) (localAddr conf) net sfile settings script
--         Nothing -> do
--           runNode (nodeName conf) (localAddr conf) net settings script

--   if isOneshot conf

--      then do let settings0 = defaultDManagerSettings
--              (settings, await) <- awaitNetwork settings0 (Just 1000000)
--              let script i man = do
--                    await >>= \case
--                      False -> putStrLn "Network timeout. Continuing in offline mode..."
--                      _ -> return ()
--                    Counter s0 <- carol man queryT
--                    putStrLn $ "Starting balance: $" <> show s0 <> "."
--                    d <- carol man $ deposit 10
--                    case d of
--                      Right n -> putStrLn $ "Deposited $" <> show n <> "."
--                      Left e -> putStrLn e
--                    Counter s1 <- carol man $ queryT
--                    putStrLn $ "Ending balance: $" <> show s1 <> "."
--              runNode' settings script

--      else do (eventChan, onUpdate, onMessage) <- mkUpdateChan
--              let script i man = do 
--                    initStore <- carol man $ queryT
--                    runUi initStore man eventChan
--                  settings = defaultDManagerSettings { onValUpdate = onUpdate
--                                                     , onGetBroadcast = onMessage }
--              runNode' settings script

