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
import CARD.LQ.Bank

main :: IO ()
main = do
  args <- getArgs
  case args of
    [idA,destA,idB,destB,selfId,selfPort] -> do 
      let src = mkSrc $ read selfPort
      chan <- mkListener src
      destA' <- mkDest destA
      destB' <- mkDest destB  
      let conf = RepConfig 
                   (read selfId) 
                   src 
                   chan 
                   (initCA (Counter 0)) 
                   SetEG
                   (Map.fromList [(read idA, destA')
                                 ,(read idB, destB')]) :: RepConfig Int SetEG (CA Int Counter) HttpT
          rep = RepRT conf (initCA (Counter 0)) empty
          script = do
            grantAll
            cmd <- words <$> lift getLine
            case cmd of
              ["dp",n] -> do r <- runOp $ deposit (read n)
                             case r of
                               Left s -> lift $ putStrLn s
                               _ -> lift $ putStrLn "Done."
              ["wd",n] -> do r <- runOp $ withdraw (read n)
                             case r of
                               Left s -> lift $ putStrLn s
                               _ -> lift $ putStrLn "Done."
              ["check"] -> lift.putStrLn.show =<< runOp current
              ["check","exact"] -> lift.putStrLn.show =<< runOp currentS
              _ -> lift $ putStrLn "Try again."
            script
      runStateT script rep
      return ()

mkListener :: Src HttpT -> IO (TChan (CMsg Int SetEG (CA Int Counter)))
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
