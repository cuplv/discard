{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.IO
import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Identity

import CARD.EventGraph
import CARD.EventGraph.SetEG
import CARD.Store
import CARD.Operation
import CARD.STM

main = test2Replicas

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

-- data Counter = Add Int | Sub Int deriving (Show,Eq,Ord)

-- instance Monoid Counter where
--   mempty = Add 0
--   mappend (Add n1) (Add n2) = Add (n1 + n2)
--   mappend (Add n1) (Sub n2) = Add (n1 - n2)
--   mappend (Sub n1) (Add n2) = Sub (n1 - n2)
--   mappend (Sub n1) (Sub n2) = Sub (n1 + n2)

-- instance Effect Counter where
--   type Store Counter = Int
--   eval [] = 0
--   eval ((Add n):es) = n + (eval es)
--   eval ((Sub n):es) = (-n) + (eval es)

-- data Broadcast g e = Broadcast String (g e)

-- data Command g e a = Deliver (Broadcast g e) 
--                    | Execute (Op e a)

-- origin (Deliver (Broadcast s _)) = Just s
-- origin _ = Nothing

------------------------------------------------------------------------

-- data RepOut g e a = RepOut 
--   String -- ^ Replica name, for logging
--   (TChan (Broadcast g e)) -- ^ Out-channel for broadcasts
--   (TChan a) -- ^ Out-channel for return values
--   (TChan String) -- ^ Out-channel for debug logs

-- replicaName (RepOut s _ _ _) = s

-- data RepIn g e a = RepIn 
--   (TChan (Command g e a)) -- ^ In-channel for commands

-- write :: (MonadIO m) => TChan a -> a -> m ()
-- write c = liftIO . atomically . writeTChan c

-- handleComm :: (EGMonad g e m, Effect e, MonadIO m, Show (g e), Show a)
--            => RepOut g e a
--            -> Command g e a -- ^ Command to be handled
--            -> StoreT g e m ()
-- handleComm (RepOut name brc retc dbgc) comm = 
--   case comm of
--     Deliver (Broadcast _ g) -> do
--       -- liftIO $ putStrLn "Handling delivery..."
--       deliver g
--       write dbgc $ name ++ ": Delivered " ++ show g
--     Execute o -> do
--       -- liftIO $ putStrLn "Handling invocation..."
--       a <- invoke o
--       g' <- history
--       -- liftIO $ putStrLn "Executed invocation."
--       write dbgc $ name ++ ": Stepped to " ++ show g'
--       write dbgc $ name ++ ": Returned " ++ show a
--       write brc (Broadcast name g')
--       write retc a

-- loopReplica :: (EGMonad g e m, Effect e, MonadIO m, Show (g e), Show a)
--             => RepIn g e a
--             -> RepOut g e a
--             -> StoreT g e m ()
-- loopReplica inc@(RepIn commc) outc = do
--   -- liftIO $ putStrLn ("Running a loop...")
--   comm <- liftIO . atomically . readTChan $ commc
--   if origin comm == Just (replicaName outc)
--      then return ()
--      else handleComm outc comm
--   loopReplica inc outc

-- joinChans :: (a -> b) -> (TChan a) -> (TChan b) -> IO ThreadId
-- joinChans f i o = joinChansIO (return . f) i o

-- joinChansIO :: (a -> IO b) -> (TChan a) -> (TChan b) -> IO ThreadId
-- joinChansIO f i o = do cin <- atomically $ dupTChan i
--                        cout <- atomically $ dupTChan o
--                        let rc = do a <- atomically $ readTChan cin
--                                    b <- f a
--                                    atomically $ writeTChan cout b
--                                    rc
--                        forkIO rc

-- consume :: (a -> IO ()) -> (TChan a) -> IO ThreadId
-- consume f i = do cin <- atomically $ dupTChan i
--                  let rc = (atomically $ readTChan cin) >>= f >> rc 
--                  forkIO rc

-- initReplica :: (EGMonad g e IO, Effect e, Show (g e), Show a)
--             => String -- ^ Replica name, for logging
--             -> (TChan (Broadcast g e)) -- ^ Broadcast channel (in-out)
--             -- ^ In-channel for commands and out-channel for return values
--             -> IO (TChan (Op e a), TChan a) 
-- initReplica name brc = do
--   inc <- newTChanIO
--   dbgc <- newTChanIO
--   retc <- newTChanIO
--   outbr <- atomically $ dupTChan brc
--   commc <- newTChanIO
--   liftIO $ joinChans Execute inc commc
--   liftIO $ joinChans Deliver brc commc
--   consume putStrLn dbgc
--   let rout = RepOut name outbr retc dbgc
--       rin = RepIn commc
--   forkIO (evalStateT (loopReplica rin rout) initRep)
--   return (inc,retc)

-- test1Replica :: IO ()
-- test1Replica = do brc <- newTChanIO :: IO (TChan (Broadcast SetEG Counter))
--                   (cin,cout) <- initReplica "R1" brc
--                   consume (\a -> putStrLn $ "R1 returned " ++ show a) cout
--                   let w :: Op Counter Int -> IO ()
--                       w = atomically . writeTChan cin
--                   atomically . writeTChan brc $ (Broadcast "R2" (tes [Add 3 <: Add 2 <# Add 1]))
--                   threadDelay (ms 3)
--                   w (const (Add 1,1))
--                   w (const (Add 1,1))
--                   w (const (Sub 3,3))
--                   threadDelay (ms 2)
--                   w (const (Add 10,10))
--                   w (\(Tally v) -> (Add 0,v))
--                   threadDelay (ms 5)
--                   return ()

-- test2Replicas :: IO ()
-- test2Replicas = do
--   brc <- newTChanIO :: IO (TChan (Broadcast SetEG Counter))
--   (cin1,cout1) <- initReplica "R1" brc
--   (cin2,cout2) <- initReplica "R2" brc
--   consume (\a -> putStrLn $ "R1 returned " ++ show a) cout1
--   consume (\a -> putStrLn $ "R2 returned " ++ show a) cout2
--   let w1 :: Op Counter Int -> IO ()
--       w1 = atomically . writeTChan cin1
--       w2 = atomically . writeTChan cin2
--   w1 $ const (Add 1,1)
--   threadDelay (ms 3)
--   w1 $ const (Add 2,2)
--   w2 $ const (Add 3,3)
--   threadDelay (ms 1)
--   w1 $ \(Tally v) -> (Add 0,v)
--   w2 $ \(Tally v) -> (Add 0,v)
--   threadDelay (ms 1)
--   return ()

add :: Int -> FrOp Counter Int
add n = LTerm (const (Effect [Add n], n))

sub :: Int -> FrOp Counter Int
sub n = LTerm (const (Effect [Sub n], n))

balance :: FrOp Counter Int
balance = LTerm (\(Counter b) -> (Effect [], b))

test2Replicas :: IO ()
test2Replicas = do
  brc <- newBroadcastTChanIO
  let mkRep :: String -> RFace String Counter SetEG (BChan Counter SetEG IO) Int IO () -> IO ThreadId
      mkRep rid script = 
        forkIO $ putStrLn (rid ++ " starting...") >> runNode rid brc script id
  mkRep "R1" $ do
    liftIO . putStrLn $ "Hello."
    rinvoke balance >>= liftIO . putStrLn . ("[R1] Balance is " ++) . show >> (liftIO $ hFlush stdout)
    liftIO $ threadDelay (ms 1)
    rinvoke (add 10)
    rinvoke (sub 9)
    rinvoke balance >>= liftIO . putStrLn . ("[R1] Balance is " ++) . show >> (liftIO $ hFlush stdout)
    liftIO $ threadDelay (ms 2)
  mkRep "R2" $ do
    liftIO $ threadDelay (ms 2)
    rinvoke balance >>= liftIO . putStrLn . ("[R2] Balance is " ++) . show
    return ()
  threadDelay (ms 5)
  return ()
