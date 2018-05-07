{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.IO
import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Identity
import Data.Time.Clock.System

import CARD.Prelude
import CARD.EventGraph.Ipfs2

main = testIpfsReplica

reader :: String -> TChan String -> IO ()
reader name c = atomically (readTChan c) 
                >>= putStrLn . (\m -> name ++ ": " ++ m) 
                >> reader name c

ms = (* 1000000)

add :: Int -> FrOp Counter Int
add n = LTerm (const (Effect [Add n], n))

sub :: Int -> FrOp Counter Int
sub n = LTerm (const (Effect [Sub n], n))

balance :: FrOp Counter Int
balance = LTerm (\(Counter b) -> (Effect [], b))

app :: String -> FrOp StrLog String
app s = LTerm (const (Effect [App s], s))

readlog :: FrOp StrLog String
readlog = LTerm (\(StrLog s) -> (Effect [], s))

testIpfs = id -- runIpfsM "/ip4/127.0.0.1/tcp/5001" "./node1"

testIpfsReplica :: IO ()
testIpfsReplica = do
  brc <- newBroadcastTChanIO
  let mkRep :: String -> RFace String Counter SetEG (BChan String Counter SetEG IO) Int IO () -> IO ThreadId
      mkRep rid script = 
        forkIO $ putStrLn (rid ++ " starting...") >> runNode rid brc script testIpfs
      reportBalance rid = rinvoke balance
                          >>= liftIO . putStrLn . (("[" ++ rid ++ "] Balance is ") ++) . show 
                          >> (liftIO $ hFlush stdout)
  mkRep "R1" $ do let exper n = do start <- lift $ systemNanoseconds <$> getSystemTime
                                   result <- rinvoke . add $ n
                                   end <- lift $ systemNanoseconds <$> getSystemTime
                                   let secs = (end - start) `div` 1000000
                                   lift $ appendFile "log.txt" (show n ++ " " ++ show secs ++ "\n")
                                   return result
                  reportBalance "R1"
                  mapM_ exper [1..50]
                  reportBalance "R1"
  threadDelay (ms 85)
  return ()
