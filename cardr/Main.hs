{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.IO
import System.Exit
import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.STM hiding (check)
import Control.Monad.Identity
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Client
import Network.Wai
import Network.Wai.Handler.Warp
import System.Environment
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import Data.Yaml
import Data.Aeson.Types (FromJSONKey)
import Options.Applicative

import CARD
import CARD.LQ.Bank
import CARD.EventGraph.Ipfs
import Storage.Ipfs.Http (mkIpfsHttp)

defaultPort = 23001 :: Int

main :: IO ()
main = node

data NetConf i = NetConf (Map i (String, Int))

others :: (Ord i) => i -> NetConf i -> [(i,(String,Int))]
others i (NetConf m) = filter (\(i',_) -> i /= i') (Map.toList m)

self i (NetConf m) = Map.lookup i m

instance (Ord i, FromJSONKey i, FromJSON i) => FromJSON (NetConf i) where
  parseJSON = withArray "NodeList" $ \v -> NetConf <$> do
    foldM unpack Map.empty v
    where unpack m = withObject "Node" $ \v -> do
            name <- v .: "name"
            host <- v .:? "host" .!= "localhost"
            port <- v .:? "port" .!= defaultPort
            return (Map.insert name (host,port) m)

data ConfCLI = ConfCLI
  { confFile :: FilePath
  , nodeName :: String }

confCLI :: IO ConfCLI
confCLI = execParser $ 
  let parser = ConfCLI
        <$> strOption (short 'c' 
                       <> metavar "FILE" 
                       <> help "Network configuration file")
        <*> strOption (short 'i' 
                       <> metavar "NAME" 
                       <> help "Node name")
      misc = (fullDesc
              <> progDesc "Run a bank account CARD node"
              <> header "cardr - a CARD demo")
  in info (parser <**> helper) misc

node :: IO ()
node = do
  conf <- confCLI
  net <- decodeFileEither (confFile conf) >>= \case
    Right net -> return net
    Left exc -> print exc >> die "Could not read network configuration file"
  let i = nodeName conf
  port <- case self i net of
            Just (_,port) -> return port
            Nothing -> die "Given node name is not in network configuration."
  let (otherIds,otherLocs) = unzip (others i net)
  ipfsr <- IpfsEG <$> mkIpfsHttp "localhost" "5001"
  httpMan <- mkMan
  otherDests <- mapM (mkDest httpMan) otherLocs
  (inbox,result,latest) <- initManager i otherIds otherDests ipfsr (Counter 0)
  let runOp' = runOp inbox result latest
  mkListener (mkSrc port) inbox
  bankScript i inbox result latest
  return ()

-- bankScript :: (Op Counter a -> IO (Either EvalFail a)) -> IO ()
bankScript name inbox result latest = do
  liftIO $ putStr (name ++ " $ ") >> hFlush stdout
  cmd <- words <$> getLine
  case cmd of
    ["dp",n] -> runOp inbox result latest (deposit $ read n) >>= (\_ -> return ())
    ["wd",n] -> runOp inbox result latest (withdraw $ read n) >>= (\_ -> return ())
    ["check"] -> runOp inbox result latest current >>= (putStrLn.show)
    ["check","exact"] -> runOp inbox result latest currentS >>= (putStrLn.show)
    _ -> putStrLn "Try again."

  bankScript name inbox result latest

-- reps :: IO ()
-- reps = do
--   args <- getArgs
--   mapi <- Right <$> mkIpfsHttp "localhost" "5001"
--   let egr = case mapi of
--               Right api -> IpfsEG api
--   case args of
--     [idA,destA,idB,destB,selfId,selfPort] -> do 
--       let src = mkSrc $ read selfPort
--       chan <- mkListener src
--       destA' <- mkDest destA
--       destB' <- mkDest destB  
--       -- let conf = RepConfig 
--       --              (read selfId) 
--       --              src 
--       --              chan 
--       --              (initCA (Counter 0)) 
--       --              egr
--       --              (Map.fromList [(read idA, destA')
--       --                            ,(read idB, destB')]) :: RepConfig Int IpfsEG (CA Int Counter) HttpT
--       initManager (read idA)
--       let rep = RepRT conf (initCA (Counter 0)) empty Map.empty
--           script = do
--             grantAll
--             liftIO $ putStr "> " >> hFlush stdout
--             cmd <- words <$> lift getLine
--             case cmd of
--               ["dp",n] -> do r <- runOp $ deposit (read n)
--                              case r of
--                                Left s -> lift $ putStrLn s
--                                _ -> lift $ putStrLn "Done."
--               ["wd",n] -> do r <- runOp $ withdraw (read n)
--                              case r of
--                                Left s -> lift $ putStrLn s
--                                _ -> lift $ putStrLn "Done."
--               ["check"] -> lift.putStrLn.show =<< runOp current
--               ["check","exact"] -> lift.putStrLn.show =<< runOp currentS
--               _ -> lift $ putStrLn "Try again."
--             script
--       runStateT script rep
--       return ()

mkListener :: (Carries HttpT (Locks i s, Hist i r s))
           => Src HttpT 
           -> TQueue (Either (BMsg (Locks i s, Hist i r s)) j) 
           -> IO ThreadId
mkListener src q = 
  forkIO (listen src (\m -> atomically (writeTQueue q (Left m)) 
                            >> return True))

-- mkListener :: Src HttpT -> IO (TChan (CMsg Int IpfsEG (CA Int Counter)))
-- mkListener src = do
--   chan <- atomically newTChan
--   forkIO (listen src (\m -> atomically (writeTChan chan m) >> return True))
--   return chan

mkMan :: IO Manager
mkMan = newManager defaultManagerSettings

mkDest :: Manager -> (String,Port) -> IO (Dest HttpT)
mkDest man (host,port) = HttpDest 
  <$> pure man 
  <*> parseRequest ("http://" ++ host ++ ":" ++ show port)

mkSrc :: Port -> Src HttpT
mkSrc = HttpSrc
