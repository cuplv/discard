{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.IO
import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Identity

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

testIpfs = runIpfsM "/ip4/127.0.0.1/tcp/5001" "./node1"

testIpfsReplica :: IO ()
testIpfsReplica = do
  brc <- newBroadcastTChanIO
  let mkRep :: String -> RFace (FrRep String IpfsEG Counter) (BChan String Counter IpfsEG (IpfsM (String, Effect Counter))) Int IO () -> IO ThreadId
      mkRep rid script = 
        forkIO $ putStrLn (rid ++ " starting...") >> runNode rid brc script testIpfs
      reportBalance rid = rinvoke balance 
                          >>= liftIO . putStrLn . (("[" ++ rid ++ "] Balance is ") ++) . show 
                          >> (liftIO $ hFlush stdout)
  mkRep "R1" $ do reportBalance "R1"
                  mapM_ (rinvoke . add) [1..50]
                  reportBalance "R1"
  threadDelay (ms 120)
  return ()
