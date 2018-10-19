{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.IO
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

import CARD
import CARD.EventGraph.SetEG
import CARD.Store.CA

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["send",destStr,port] -> do 

      dest <- mkDest destStr
      let src = mkSrc $ read port
      chan <- mkListener src

      let conf = RepConfig 1 src chan (Counter 0) SetEG (Map.fromList [(1,dest)])
          rep = RepRT conf (Counter 0) empty
          step = do
            n <- read <$> lift getLine
            emit (ef$ Add n)
            lift . putStrLn . ("Store is now " ++) . show =<< check
            step

      runStateT step rep
      return ()

    ["listen",destStr,port,thr,extra] -> do
      dest <- mkDest destStr
      let src = mkSrc $ read port
          extra' = read extra :: Int
      chan <- mkListener src
      let conf = RepConfig 0 src chan (Counter 0) SetEG (Map.fromList [(0,dest)])
          rep = RepRT conf (Counter 0) empty
      let step = do
            update
            emit (ef$ Add extra')
            step
          script = do
            lift$ putStrLn $ "Waiting until store is >= " ++ thr
            s <- await (\(Counter n) -> n >= 100)
            lift$ putStrLn ("Reached, store is " ++ show s)
            step

      runStateT script rep
      return ()

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
