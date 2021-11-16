{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.Discard.Node
  ( Script
  , ManagerConn (..)
  , DManagerSettings (..)
  , defaultDManagerSettings
  , defaultDManagerSettings'
  , awaitNetwork
  , runNode
  , runNode'
  , runNodeFile
  , runCCRT
  ) where

import System.IO
import System.Exit
import System.Directory (doesFileExist)
import Data.List (genericLength)
import Control.Monad
import Control.Monad.Trans (MonadIO,liftIO)
import Control.Concurrent (forkIO,ThreadId,threadDelay,killThread)
import Control.Concurrent.STM hiding (check)
import Network.HTTP.Client
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Yaml
import Data.Aeson.Types (ToJSONKey, FromJSONKey)
import System.Random

import Data.CvRDT
import Data.CARD.Store
import Data.EventGraph (Edge,empty)

import Network.Discard.Broadcast
import Network.Discard.RepCard
import Data.EventGraph.Ipfs (IpfsEG,mkIpfsEG'')

-- | A program to run on a replica node, which has access to the
-- state-management thread via the 'ManagerConn' in order to execute
-- non-trivial operations.
type Script r q h i c e s a =
  i 
  -> ManagerConn q h i c e s
  -> IO a

-- | Run a replica node, explicitly providing an initial state.  Make
-- sure the initial store value matches the initial store history!
runNode' :: (Ord s, ManC (IpfsEG i) q h i c e s, ToJSON (h (i,e)), ToJSON q, FromJSON q, ToJSON i, ToJSONKey i, ToJSONKey c, ToJSON c, ToJSON e, FromJSONKey i, FromJSON i, FromJSONKey c, FromJSON c, FromJSON e, FromJSON (h (i, e)), h ~ Edge (IpfsEG i))
         => i  -- ^ Name
         -> String -- ^ IPFS API URI
         -> NetConf i -- ^ Replica network
         -> s -- ^ Initial store value
         -> Store q h i c e -- ^ Initial store (locks + history + res)
         -> DManagerSettings q h i c e s
         -> Script (IpfsEG i) q h i c e s a -- ^ Actions to perform
         -> IO (a, (s, Store q h i c e))
runNode' i ipfsAddr net val0 store0 dmsets script = do
  port <- case self i net of
            Just (_,port) -> return port
            Nothing -> die "Given node name is not in network configuration."
  let (otherIds,otherLocs) = unzip (others i net)
  ipfsr <- mkIpfsEG'' ipfsAddr i (dmsDebugLevel dmsets)
  httpMan <- mkMan
  otherDests <- mapM (mkDest httpMan) otherLocs
  man <- initManager i otherIds otherDests ipfsr val0 store0 dmsets
  lt <- mkListener (dmsDebugLevel dmsets) (mkSrc port) man
  res <- script i man
  killThread lt
  stateFinal <- killManager man
  return (res, stateFinal)

-- | Run a complete replica node, establishing network and state
-- management threads, and execute a replica script on it.
--
-- If/when the script terminates, the replica node will be torn down
-- and background threads killed.  The node will start with an empty
-- history and will be brought up-to-date by broadcasts from the
-- network.
runNode :: (Ord s, ManC (IpfsEG i) q h i c e s, ToJSON (h (i,e)), ToJSON q, FromJSON q, ToJSON i, ToJSONKey i, ToJSONKey c, ToJSON c, ToJSON e, FromJSONKey i, FromJSON i, FromJSONKey c, FromJSON c, FromJSON e, FromJSON (h (i,e)), h ~ Edge (IpfsEG i))
        => i -- ^ name
        -> String -- ^ IPFS API URI
        -> NetConf i -- ^ Replica network
        -> DManagerSettings q h i c e s
        -> Script (IpfsEG i) q h i c e s a
        -> IO a
runNode i ipfsAddr net dmsets script = 
  fst <$> runNode' i ipfsAddr net val0 store0 dmsets script
  where val0 = baseStoreValue dmsets
        q0 = dmsBaseReqState dmsets
        store0 = (q0, (mempty, Data.EventGraph.empty))

-- | Run a node, loading the initial state from the given file (if it
-- exists) and writing the final state to the file on exit (creating
-- it if it does not exist).
runNodeFile :: (Ord s, ManC (IpfsEG i) q h i c e s, Monoid q, ToJSON (h (i,e)), ToJSON q, FromJSON q, ToJSON i, ToJSONKey i, ToJSONKey c, ToJSON c, ToJSON e, FromJSONKey i, FromJSON i, FromJSONKey c, FromJSON c, FromJSON e, FromJSON (h (i,e)), h ~ Edge (IpfsEG i), FromJSON s, ToJSON s)
            => i -- ^ Name
            -> String -- ^ IPFS API address
            -> NetConf i -- ^ Replica network
            -> FilePath -- ^ Save-file path
            -> DManagerSettings q h i c e s
            -> Script (IpfsEG i) q h i c e s a -- ^ Actions to perform
            -> IO a
runNodeFile i ipfsAddr net sfile dmsets script = do
  let trypfile = doesFileExist sfile
  (val0,store0) <- doesFileExist sfile >>= \case
    True -> decodeFileEither sfile >>= \case
      Right state -> return state
      Left e -> do print e
                   die $ "Save file \"" <> sfile <> "\" exists but is unreadable."
    False -> return (baseStoreValue dmsets, (mempty, (mempty,Data.EventGraph.empty)))
  (a,stateFinal) <- runNode' i ipfsAddr net val0 store0 dmsets script
  encodeFile sfile stateFinal
  return a

mkListener :: (Carries HttpT (Store q h i c e))
           => Int 
           -> Src HttpT 
           -> ManagerConn q h i c e s
           -> IO ThreadId
mkListener dbl src man = 
  forkIO (listen dbl src handle)
  where handle m = case m of
          Hello -> do s <- getLatestStore man
                      giveUpdate m man
                      return (True,Just (BCast s))
          m -> giveUpdate m man >> return (True, Nothing)

mkMan :: IO Manager
mkMan = newManager defaultManagerSettings

mkDest :: Manager -> (String,Int) -> IO (Dest HttpT)
mkDest man (host,port) = HttpDest 
  <$> pure man 
  <*> parseRequest ("http://" ++ host ++ ":" ++ show port)

mkSrc :: Int -> Src HttpT
mkSrc = HttpSrc
