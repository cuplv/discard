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
import Data.Foldable (foldl')

import CARD
import CARD.Experiment
import CARD.EventGraph.Ipfs
import CARD.LQ.Bank
import CARD.Node

------------------------------------------------------------------------

data ServerConf i = ServerConf
  { serverId :: i
  , serverPort :: Int
  , ipfsPort :: Int }

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
        <*> option auto (long "ipfs-port" <> value 5001)

      misc = (fullDesc <> progDesc "Run an experiment node")
  in info (parser <**> helper) misc

main :: IO ()
main = experimentNode =<< confCLI

experimentNode :: ServerConf String -> IO ()
experimentNode conf = do
  lastv <- newTVarIO Nothing
  resultsv <- newTVarIO Map.empty
  let sets = setHost "!6" . setPort (serverPort conf) $ defaultSettings
  runSettings sets 
              (cmdGetter (serverId conf) 
                         (startExp (serverId conf) (ipfsPort conf) lastv resultsv) 
                         (resultsExp resultsv))

startExp :: String 
         -> Int 
         -> TVar (Maybe Int)
         -> TVar (Map Int ExpResult) 
         -> (ExpConf, NetConf String) 
         -> IO (Maybe Int)
startExp i ipfsPort lastv resultsv (ec,nc) = do
  mcurrent <- atomically (readTVar lastv) >>= \case
    Nothing -> return (Just 0)
    Just last -> (Map.lookup last <$> readTVarIO resultsv) >>= \case
      Nothing -> return Nothing
      Just _ -> return (Just (last + 1))
  let s0 = Counter 100000
      tsize = 100000 -- 100ms
  case mcurrent of
    Just current -> do
      atomically $ swapTVar lastv (Just current)
      -- Start the experiment
      forkIO $ do 
        results <- runNode i (ipfsPort) nc s0 tsize (expScript ec)
        atomically $ modifyTVar resultsv (Map.insert current results)
        putStrLn $ "Finished experiment " ++ show current
      putStrLn $ "Started experiment " ++ show current
      putStrLn $ "Time: " ++ show (expTime ec) ++ " s"
      putStrLn $ "Rate: " ++ show (expRate ec) ++ " req/s"
  return mcurrent



bankProfile :: Profile Counter
bankProfile = \case
  "deposit" -> deposit 10 >> return ()
  "withdraw" -> withdraw 1 >> return ()
  "current" -> current >> return ()
  "currentS" -> currentS >> return ()

-- | For our experiments, we define infinity as 10 seconds.  (This
-- means that operations which are cut off by the end of the
-- experiment are assumed to have taken 10 seconds)
infinity :: NominalDiffTime
infinity = fromRational 10

oneSec :: Int -- Microseconds
oneSec = 1000000

stamp :: n -> TQueue (n,UTCTime) -> IO ()
stamp n q = do tm <- getCurrentTime
               atomically (writeTQueue q (n,tm))

timeConv = fromRational.toRational

expScript :: ExpConf -> Script String (IpfsEG i) Counter ExpResult
expScript econf i man = do
  startQ <- newTQueueIO :: IO (TQueue ((Int,String),UTCTime))
  endQ <- newTQueueIO :: IO (TQueue (Int,UTCTime))
  reqs <- zip [0..]
          . map (chooseOp bankProfile (expMix econf))
          . randomRs (1,100) 
          <$> getStdGen :: IO [(Int,(String,LQ Counter ()))]
  let mkRequest (n,(s,t)) = stamp (n,s) startQ >> runLQM man (const $ stamp n endQ) t
      rc startTime (r:rs) = do 
        mkRequest r
        threadDelay (oneSec `div` expRate econf)
        tm <- getCurrentTime
        if diffUTCTime tm startTime < timeConv (expTime econf)
           then rc startTime rs
           else return ()
  threadDelay (oneSec * 3) -- Wait 3s to make sure everyone is listening
  putStrLn "Starting requests..."
  startTime <- getCurrentTime
  rc startTime reqs
  putStrLn "Finished requests, waiting for stragglers..."
  threadDelay (oneSec * 10) -- Wait 10s to let everyone finish
  collectData startQ endQ

collectData :: TQueue ((Int,String),UTCTime)
            -> TQueue (Int,UTCTime) 
            -> IO ExpResult
collectData startQ endQ = do
  mStarts <- foldl' (\m ((n,s),st) -> Map.insert n (s,st,Nothing) m) Map.empty 
             <$> atomically (flushTQueue startQ)
  mAll <- foldl' (\m (n,et) -> Map.adjust (\(s,st,_) -> (s,st,Just et)) n m) mStarts
          <$> atomically (flushTQueue endQ)
  let count :: (String,UTCTime,Maybe UTCTime) -> Map String Int -> Map String Int
      count (s,_,te) = case te of
        Just _ -> Map.alter (\case
                                Just n -> Just (n + 1)
                                Nothing -> Just 1) s
        Nothing -> id
      mCounts = foldr count Map.empty mAll :: Map String Int
      countLat :: (String,UTCTime,Maybe UTCTime) -> Map String NominalDiffTime -> Map String NominalDiffTime
      countLat (s,ts,mte) = case mte of
        Just te -> Map.alter (\case
                                 Just t -> Just (t + diffUTCTime te ts)
                                 Nothing -> Just (diffUTCTime te ts)) s
        Nothing -> id
      mTotalTime = foldr countLat Map.empty mAll :: Map String NominalDiffTime
      mLats = Map.mapWithKey (\s t -> t / fromIntegral (mCounts Map.! s)) mTotalTime
      countUnf :: (String,UTCTime,Maybe UTCTime) -> Map String Int -> Map String Int
      countUnf (s,_,te) = case te of
        Nothing -> Map.alter (\case
                                 Just n -> Just (n + 1)
                                 Nothing -> Just 1) s
        Just _ -> id
      mUnfinished = foldr countUnf Map.empty mAll :: Map String Int
  return (ExpResult mLats mCounts mUnfinished)


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
      Nothing -> respond $ responseLBS status503 [] "Experiment in progress"
    Just (Report n) -> hr n >>= \case
      Just results -> respond $ responseLBS status200 [] (encode results)
      Nothing -> respond $ responseLBS status503 [] "Experiment not completed"
    Nothing -> respond $ responseLBS status400 [] "No parse"
