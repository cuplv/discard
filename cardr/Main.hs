{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.IO
import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Identity
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Client
import Network.Wai
import Network.Wai.Handler.Warp
import System.Environment

import CARD
import CARD.EventGraph.SetEG

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["send",destStr,port,e1,e2] -> do 
      let n1 = read e1 :: Int
          n2 = read e2 :: Int
      dest <- mkDest destStr
      chan <- mkListener (mkSrc $ read port)
      hist1 <- append SetEG empty (1::Int,Effect [Add n1])
      hist2 <- append SetEG hist1 (1::Int,Effect [Add n2])
      send dest (BCast 1 hist2)
      (BCast i e) <- atomically (readTChan chan)
      putStrLn.show $ e
    ["listen",destStr,port,app,conc] -> do
      dest <- mkDest destStr
      chan <- mkListener (mkSrc $ read port)
      respond dest chan app conc

respond dest chan app conc = do
  (BCast i hist) <- atomically $ readTChan chan
  putStrLn "Got message."
  hist3 <- append SetEG hist (0::Int,Effect [Add (read app)])
  hist4 <- append SetEG empty (0::Int,Effect [Add (read conc)])
  hist5 <- merge SetEG hist3 hist4
  send dest (BCast 0 hist5)
  respond dest chan app conc

mkListener :: Src HttpT -> IO (TChan (CMsg Int SetEG Counter))
mkListener src = do
  chan <- atomically newTChan
  forkIO (listen src (\m -> atomically (writeTChan chan m) >> return True))
  return chan

mkDest :: String -> IO (Dest HttpT)
mkDest t = HttpDest 
  <$> newManager defaultManagerSettings 
  <*> parseRequest t

mkSrc :: Port -> Src HttpT
mkSrc = HttpSrc
