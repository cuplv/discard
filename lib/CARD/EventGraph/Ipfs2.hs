{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | An alternate use of IPFS as an EventGraph, in which IPFS objects
-- representing events actually contain their vis-set (rather than
-- just the hashes of the IPFS objects of their vis-set).  This should
-- make 'merge' enormously faster, while maybe slowing down 'append'.
module CARD.EventGraph.Ipfs2 
  ( IpfsEG
  , IpfsM
  , runIpfsM
  ) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import System.Process (callProcess,readProcess,readCreateProcess,proc)
import System.Directory
import Data.Semigroup ((<>))

import Data.Set (Set,(\\))
import qualified Data.Set as Set

import CARD.EventGraph

payloadName = "PAYLOAD"
prevName = "PREV"

newtype EventRef e = EventRef { eventHash :: String } deriving (Show, Read, Eq, Ord)

newtype IpfsEG e = IpfsEG (Set (EventRef e)) deriving (Show, Read, Eq, Ord)

instance (Ord e) => EventGraph IpfsEG e where
  empty = IpfsEG Set.empty

data IpfsConf = IpfsConf { ipfsAPI :: String
                         , homeDir :: String }

data IpfsState e = IpfsState { fsCache :: Map (EventRef e) FilePath }

newtype IpfsM e a = IpfsM (StateT (IpfsState e) (ReaderT IpfsConf IO) a) 
                    deriving (Functor, Applicative, Monad, MonadIO)

deriving instance MonadReader IpfsConf (IpfsM e)
deriving instance MonadState (IpfsState e) (IpfsM e)

runIpfsM :: String -> FilePath -> IpfsM e a -> IO a
runIpfsM api home (IpfsM a) = 
  createDirectoryIfMissing True home
  >> runReaderT (evalStateT a (IpfsState Map.empty)) (IpfsConf api home)

checkCache :: (Ord e, Read e) => EventRef e -> IpfsM e (Maybe FilePath)
checkCache e = Map.lookup e . fsCache <$> get

addCache :: (Ord e, Read e) => EventRef e -> FilePath -> IpfsM e ()
addCache e fp = put =<< IpfsState . Map.insert e fp . fsCache <$> get 

invalCache :: EventRef e -> IpfsM e ()
invalCache e = put =<< IpfsState . Map.delete e . fsCache <$> get

askHome :: IpfsM e FilePath
askHome = homeDir <$> ask

askApi :: IpfsM e String
askApi = ipfsAPI <$> ask

inspectEv :: (Ord e, Read e) => EventRef e -> IpfsM e (e, IpfsEG e)
inspectEv e = 
  do fp <- cacheEv e
     home <- askHome
     let cachePath = (home <> "/" <> fp)
         absol p = (home <> "/") <> p
     payload <- liftIO $ read <$> readFile (absol fp <> "/" <> payloadName)
     prev <- liftIO $ listDirectory (absol fp <> "/" <> prevName)
     mapM (\p -> addCache (EventRef p) (fp <> "/" <> prevName <> "/" <> p)) prev
     return (payload, IpfsEG $ Set.fromList (map EventRef prev))

cacheEv :: (Ord e, Read e) => EventRef e -> IpfsM e FilePath
cacheEv e = do
  mfp <- checkCache e
  case mfp of
    Just fp -> return fp
    Nothing -> do let fp = eventHash e
                  home <- askHome
                  ipfs "get" [ "-o", home <> "/" <> fp
                             , "/ipfs/" <> fp]
                  addCache (EventRef fp) fp
                  return fp

lio :: (MonadIO m) => IO a -> m a 
lio = liftIO

storeEv :: (Ord e, Show e, Read e) => (e, IpfsEG e) -> IpfsM e (EventRef e)
storeEv (e,(IpfsEG g)) = do 
  home <- askHome
  api <- askApi
  let newEventDir = home <> "/" <> "new-event"
  lio$ createDirectory newEventDir
  lio$ writeFile (newEventDir <> "/" <> payloadName) (show e)
  lio$ createDirectory (newEventDir <> "/" <> prevName)
  mapM_ (consumeEv (newEventDir <> "/" <> prevName)) (Set.toList g)
  ev' <- ipfs "add" ["-rQ",newEventDir]
  lio$ removeDirectoryRecursive newEventDir
  return (EventRef . head . lines $ ev')

consumeEv :: (Ord e, Read e) => FilePath -> EventRef e -> IpfsM e ()
consumeEv newDir e = do 
  home <- askHome
  fp <- ((home <> "/") <>) <$> cacheEv e
  lio$ callProcess "mv" [fp,newDir <> "/"]
  urs <- Set.toList <$> underRefs e
  mapM_ (invalCache . EventRef) urs

ipfs :: String -> [String] -> IpfsM e String
ipfs cmd args = ipfs' cmd args ""

ipfs' :: String -> [String] -> String -> IpfsM e String
ipfs' cmd args inp = do 
  api <- askApi
  lio$ readProcess "ipfs" (["--api",api,cmd] ++ args) inp

underRefs :: EventRef e -> IpfsM e (Set String)
underRefs (EventRef e) = do
  api <- askApi
  Set.fromList . lines <$> ipfs "refs" ["/ipfs/" ++ e]

instance (Show e, Read e, Ord e) => MonadEG IpfsEG e (IpfsM e) where
  append e g = IpfsEG . Set.singleton <$> storeEv (e,g)
  merge (IpfsEG s1) (IpfsEG s2) = 
    do let underEG s = foldl' Set.union Set.empty <$> mapM underRefs (Set.toList s)
       rs1 <- (Set.map EventRef) <$> underEG s1
       rs2 <- (Set.map EventRef) <$> underEG s2
       return . IpfsEG $ Set.union (s1 \\ rs2) (s2 \\ rs1)
  edge (IpfsEG s) = mapM inspectEv (Set.toList s)
