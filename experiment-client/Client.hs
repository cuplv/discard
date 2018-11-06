{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Network.HTTP.Types
import Network.HTTP.Client
import Data.Yaml (decodeFileEither)
import Data.Aeson (encode,decode)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy.Char8 as BC
import Options.Applicative
import Control.Concurrent.STM
import Data.Map (Map)
import qualified Data.Map as Map
import System.Random
import Data.Time.Clock
import Control.Concurrent (threadDelay,forkIO)
import Data.List (genericLength)
import System.Exit

import CARD
import CARD.Experiment
import CARD.EventGraph.Ipfs
import CARD.LQ.Bank
import CARD.Node

main :: IO ()
main = do 
  conf <- confCLI
  net <- decodeFileEither (clientNetConf conf) >>= \case
    Right net -> return net
    Left _ -> die "Could not read net conf file"
  runExperiment 
    (ExpConf (clientRate conf) "asdf" (uncurry Mix (clientMix conf)) (clientTime conf))
    net

data ClientConf i = ClientConf
  { clientNetConf :: FilePath
  , clientMix :: ([(Int,String)],String)
  , clientRate :: Int
  , clientTime :: Int }

confCLI :: IO (ClientConf String)
confCLI = execParser $
  let parser = ClientConf
        <$> strOption (short 'c' <> help "net conf filepath")
        <*> option auto (long "mix")
        <*> option auto (long "rate" <> metavar "MICROSEC")
        <*> option auto (long "time" <> metavar "SEC")
      misc = (fullDesc <> progDesc "Run an experiment")
  in info (parser <**> helper) misc

runExperiment :: ExpConf -> ExpNetConf String -> IO ()
runExperiment ec enc = do
  let nc = getNetConf enc
      addrs = expAddrs enc
      cmd = Launch ec nc
  man <- newManager defaultManagerSettings
  let mkReq :: String -> IO (Int,String)
      mkReq addr = do
        ireq <- parseRequest addr
        let req = ireq { requestBody = RequestBodyLBS $ encode cmd }
        resp <- httpLbs req man
        case responseStatus $ resp of
          c | c == status200 -> case decode (responseBody resp) of
                                  Just n -> return (n,addr)
          c -> print c >> die "Oops"
  exps <- traverse mkReq addrs
  threadDelay $ (expTime ec + 10) * 1000000
  let getRes :: (Int,String) -> IO ExpResult
      getRes (n,addr) = do
        ireq <- parseRequest addr
        let req = ireq { requestBody = RequestBodyLBS $ encode (Report n :: ExpCmd String) }
        resp <- httpLbs req man
        case responseStatus $ resp of
          c | c == status200 -> case decode (responseBody resp) of
                                  Just d -> return d
  results <- traverse getRes exps
  print results
