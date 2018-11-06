{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.IO
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

import CARD
import CARD.LQ.Bank
import CARD.EventGraph.Ipfs (mkIpfsEG')

main :: IO ()
main = node


data ConfCLI = ConfCLI
  { confFile :: FilePath
  , nodeName :: String
  , expMode :: Bool
  , expMix :: String
  , expRate :: Int }

confCLI :: IO ConfCLI
confCLI = execParser $ 
  let parser = ConfCLI
        <$> strOption (short 'c' 
                       <> metavar "FILE" 
                       <> help "Network configuration file")
        <*> strOption (short 'i' 
                       <> metavar "NAME" 
                       <> help "Node name")
        <*> switch (long "experiment")
        <*> strOption (long "mix" <> value "15p")
        <*> option auto (long "rate" <> value 10000)
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
  ipfsr <- mkIpfsEG' (nodeName conf)
  httpMan <- mkMan
  otherDests <- mapM (mkDest httpMan) otherLocs
  (inbox,result,latest) <- initManager i otherIds otherDests ipfsr (Counter 0)
  mkListener (mkSrc port) inbox
  if expMode conf
     then do rand <- getStdGen
             let vals = randomRs (1,100) rand :: [Int]
             latencies <- exprScript 1000 [] 0 vals (mkMix (expMix conf)) (expRate conf) inbox result latest
             let average = ((sum latencies) / genericLength latencies) :: Double
             putStrLn $ "Average (s): " ++ show average

     else bankScript 0 i inbox result latest >> return ()
  return ()

mkMix mix = case mix of
  "15p" -> \v -> case () of
                   _ | v <= 7 -> deposit 10 >> return ()
                     | v <= 15 -> withdraw 1 >> return ()
                     | otherwise -> current >> return ()

exprScript num lates n vals mix rate inbox result latest = do
  let cmd = mix (head vals)
  startTime <- getCurrentTime
  n' <- runOp inbox result latest n cmd >>= (\(_,n) -> return (n - 1))
  endTime <- getCurrentTime
  let latency = (fromRational.toRational $ diffUTCTime endTime startTime) :: Double
  print latency
  threadDelay rate
  if num > 0
     then exprScript (num - 1) (latency:lates) n' (tail vals) mix rate inbox result latest
     else return (latency:lates)

bankScript n name inbox result latest = do
  liftIO $ putStr (name ++ " $ ") >> hFlush stdout
  cmd <- words <$> getLine
  n' <- case cmd of
          ["dp",a] -> runOp inbox result latest n (deposit $ read a) >>= (\(_,n) -> return (n - 1))
          ["wd",a] -> runOp inbox result latest n (withdraw $ read a) >>= (\(_,n) -> return (n - 1))
          ["check"] -> runOp inbox result latest n current >>= (putStrLn.show.fst) >> return n
          ["check","exact"] -> runOp inbox result latest n currentS >>= (putStrLn.show.fst) >> return n
          _ -> putStrLn "Try again." >> return n

  bankScript n' name inbox result latest

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
