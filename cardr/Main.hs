{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.STM

import Data.EventGraph
import Data.EventGraph.SetEG
import Replica

main = testBroadcast

reader :: String -> TChan String -> IO ()
reader name c = atomically (readTChan c) 
                >>= putStrLn . (\m -> name ++ ": " ++ m) 
                >> reader name c

ms = (* 1000000)

testchannels :: IO ()
testchannels = 
  do c1 <- newTChanIO
     -- Reader
     forkIO (reader "Reader" c1)
     -- Writer 1
     forkIO (threadDelay (ms 2) >> atomically (writeTChan c1 "Message #1"))
     -- Writer 2
     forkIO (threadDelay (ms 2) >> atomically (writeTChan c1 "Message #2"))
     threadDelay (ms 3)
     return ()

testBroadcast :: IO ()
testBroadcast = 
  do br <- atomically $ newBroadcastTChan
     r1 <- atomically $ dupTChan br
     r2 <- atomically $ dupTChan br
     forkIO (threadDelay (ms 2) >> reader "R1" r1)
     forkIO (threadDelay (ms 3) >> reader "R2" r2)
     atomically (writeTChan br "Test Message #1")
     atomically (writeTChan r1 "Test Message #2")
     atomically (writeTChan r2 "Test Message #3")
     threadDelay (ms 4)
     return ()

data Counter = Add Int | Sub Int

instance Monoid Counter where
  mempty = Add 0
  mappend (Add n1) (Add n2) = Add (n1 + n2)
  mappend (Add n1) (Sub n2) = Add (n1 - n2)
  mappend (Sub n1) (Add n2) = Sub (n1 - n2)
  mappend (Sub n1) (Sub n2) = Sub (n1 + n2)

instance Effect Counter where
  type Store Counter = Int
  eval [] = 0
  eval ((Add n):es) = n + (eval es)
  eval ((Sub n):es) = (-n) + (eval es)

data Broadcast g e = Broadcast (g e)

data Command g e a = Deliver (Broadcast g e) 
                   | Execute (Op e a)

------------------------------------------------------------------------

write :: (MonadIO m) => TChan a -> a -> m ()
write c = liftIO . atomically . writeTChan c

handleComm :: (EventGraph g, MonadIO (Resolver g), Show (g e), Ord e, Effect e, Show a)
           => String -- ^ Replica name, for logging
           -> TChan (Broadcast g e) -- ^ In-channel for broadcasts
                                    -- from other replicas
           -> TChan a -- ^ Out-channel for return values
           -> TChan String -- ^ Out-channel for debug logs
           -> Command g e a -- ^ Command to be handled
           -> RepS g e ()
handleComm name brc retc dbgc comm = 
  case comm of
    Deliver (Broadcast g) -> do
      deliverM g
      write dbgc $ name ++ ": Delivered " ++ show g
    Execute o -> do
      (g',a) <- invokeM o
      write dbgc $ name ++ ": Stepped to " ++ show g'
      write dbgc $ name ++ ": Returned " ++ show a
      write brc (Broadcast g')
      write retc a

