{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Data.Aeson
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

import CARD
import CARD.Experiment
import CARD.EventGraph.Ipfs
import CARD.LQ.Bank
import CARD.Node

------------------------------------------------------------------------

data ServerConf i = ServerConf
  { serverId :: i
  , serverPort :: Int }

confCLI :: IO (ServerConf String)
confCLI = execParser $
  let parser = ServerConf
        <$> strOption (short 'i' <> long "name" <> help "node id/name")
        <*> option 
              auto 
              (short 'p' 
               <> long "port" 
               <> help ("port for control server to listen on (default " 
                        ++ show defaultServerPort ++ ")") 
               <> value defaultServerPort)

      misc = (fullDesc <> progDesc "Run an experiment node")
  in info (parser <**> helper) misc

main :: IO ()
main = experimentNode =<< confCLI

experimentNode :: ServerConf String -> IO ()
experimentNode conf = do
  lastv <- newTVarIO 0
  resultsv <- newTVarIO (Map.fromList [(0,ExpResult (-1.0))])
  let sets = setHost "!6" . setPort (serverPort conf) $ defaultSettings
  runSettings sets 
              (cmdGetter (serverId conf) 
                         (startExp (serverId conf) lastv resultsv) 
                         (resultsExp resultsv))

startExp :: String -> TVar Int -> TVar (Map Int ExpResult) -> (ExpConf, NetConf String) -> IO (Maybe Int)
startExp i lastv resultsv (ec,nc) = do
  last <- readTVarIO lastv
  (Map.lookup last <$> readTVarIO resultsv) >>= \case
     Nothing -> return Nothing
     Just _ -> do
       let current = last + 1
       atomically $ swapTVar lastv current
       -- Start the experiment
       forkIO $ do 
         avg <- runNode i nc (expScript ec)
         atomically $ modifyTVar resultsv (Map.insert current avg)
         putStrLn $ "Finished experiment " ++ show current
       putStrLn $ "Started experiment " ++ show current
       return (Just current)

bankProfile :: Profile Counter
bankProfile = \case
  "deposit" -> deposit 10 >> return ()
  "withdraw" -> withdraw 1 >> return ()
  "current" -> current >> return ()

expScript :: ExpConf -> Script String ExpResult
expScript econf n name inbox result latest = do
  rand <- getStdGen
  let vals = randomRs (1,100) rand :: [Int]
  -- Wait 1 second to make sure everyone is listening
  threadDelay 1000000
  expStartTime <- getCurrentTime
  let recScript n lates vals = do
        let op = chooseOp bankProfile (expMix econf) (head vals)
        opStartTime <- getCurrentTime
        n' <- runOp inbox result latest n op >>= (\(_,n) -> return (n - 1))
        opEndTime <- getCurrentTime
        let latency = (fromRational.toRational $ diffUTCTime opEndTime opStartTime) :: Double
            lates' = latency:lates
        threadDelay (expRate econf)
        if diffUTCTime opEndTime expStartTime > (fromRational.toRational $ expTime econf)
           then return lates'
           else recScript n' lates' (tail vals)
  lates <- recScript 0 [] vals
  let average = ((sum lates) / genericLength lates) :: Double
  -- Wait 5 seconds for other nodes to finish
  threadDelay 5000000
  return $ ExpResult average


resultsExp :: TVar (Map Int ExpResult) -> Int -> IO (Maybe ExpResult)
resultsExp resultsv n = Map.lookup n <$> readTVarIO resultsv

cmdGetter :: (Ord i, ToJSON i, FromJSON i) 
          => i
          -> ((ExpConf, NetConf i) -> IO (Maybe Int))
          -> (Int -> IO (Maybe ExpResult))
          -> Application
cmdGetter _ hl hr request respond = do
  body <- strictRequestBody request
  case decode body of
    Just (Launch ec nc) -> hl (ec,nc) >>= \case
      Just n -> respond $ responseLBS status200 [] (BC.pack $ show n)
      Nothing -> respond $ responseLBS status503 [] "Previous experiment in progress"
    Just (Report n) -> hr n >>= \case
      Just results -> respond $ responseLBS status200 [] (encode results)
      Nothing -> respond $ responseLBS status503 [] "Experiment not completed"
    Nothing -> respond $ responseLBS status400 [] "No parse"
