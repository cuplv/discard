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
import CARD.Store.Bank

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
              ["dp",n] -> do r <- deposit (read n)
                             case r of
                               Left s -> lift $ putStrLn s
                               _ -> lift $ putStrLn "Done."
              ["wd",n] -> do r <- withdraw (read n)
                             case r of
                               Left s -> lift $ putStrLn s
                               _ -> lift $ putStrLn "Done."
              ["check"] -> lift.putStrLn.show =<< current
              ["check","exact"] -> lift.putStrLn.show =<< currentS
              _ -> lift $ putStrLn "Try again."
            script
      runStateT script rep
      return ()

  -- case args of
  --   ["send",destStr,port] -> do 

  --     dest <- mkDest destStr
  --     let src = mkSrc $ read port
  --     chan <- mkListener src

  --     let conf = RepConfig 1 src chan (Counter 0) SetEG (Map.fromList [(1,dest)])
  --         rep = RepRT conf (Counter 0) empty
  --         step = do
  --           n <- read <$> lift getLine
  --           emit (ef$ Add n)
  --           lift . putStrLn . ("Store is now " ++) . show =<< check
  --           step

  --     runStateT step rep
  --     return ()

  --   ["listen",destStr,port,thr,extra] -> do
  --     dest <- mkDest destStr
  --     let src = mkSrc $ read port
  --         extra' = read extra :: Int
  --     chan <- mkListener src
  --     let conf = RepConfig 0 src chan (Counter 0) SetEG (Map.fromList [(0,dest)])
  --         rep = RepRT conf (Counter 0) empty
  --     let step = do
  --           update
  --           emit (ef$ Add extra')
  --           step
  --         script = do
  --           lift$ putStrLn $ "Waiting until store is >= " ++ thr
  --           s <- await (\(Counter n) -> n >= 100)
  --           lift$ putStrLn ("Reached, store is " ++ show s)
  --           step

  --     runStateT script rep
  --     return ()

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
