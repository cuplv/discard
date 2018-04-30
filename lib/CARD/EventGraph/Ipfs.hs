{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module CARD.EventGraph.Ipfs where

import Control.Monad
import Control.Monad.Reader
import Data.Foldable (foldl')
-- import qualified System.IO as IO (hFlush,stdout)
-- import Turtle
-- import Turtle.Prelude (cd,ls,mv)
-- import Control.Foldl (list)
-- import Data.Text (Text,pack,unpack,stripEnd)

import System.Process (readProcess)

import Data.Semigroup ((<>))

import Data.Set (Set)
import qualified Data.Set as Set

import CARD.EventGraph

-- | An IPFS object
newtype EventRef e = EventRef String deriving (Show, Read, Eq, Ord)

newtype IpfsEG e = IpfsEG (Set (EventRef e)) deriving (Show, Read, Eq, Ord)

instance (Ord e) => EventGraph IpfsEG e where
  empty = IpfsEG Set.empty
  
data Event e = Event e (IpfsEG e) deriving (Show, Read, Eq, Ord)

data IpfsConf = IpfsConf { ipfsAPI :: String }

newtype IpfsM a = IpfsM (ReaderT IpfsConf IO a) 
                  deriving (Functor, Applicative, Monad, MonadIO)

runIpfsM :: String -> IpfsM a -> IO a
runIpfsM api (IpfsM r) = runReaderT r (IpfsConf api)

deriving instance MonadReader IpfsConf IpfsM

storeEv :: (Ord e, Show e) => Event e -> IpfsM (EventRef e)
storeEv e = do api <- ipfsAPI <$> ask
               EventRef <$> liftIO (readProcess 
                 "ipfs" 
                 ["--api",api,"add","-Q"] 
                 (show e))

fetchEv :: (Ord e, Read e) => EventRef e -> IpfsM (Event e)
fetchEv (EventRef s) = do api <- ipfsAPI <$> ask
                          read <$> liftIO (readProcess 
                            "ipfs" 
                            ["--api",api,"cat"]
                            ("/ipfs/"++ s))

instance (Show e, Read e, Ord e) => MonadEG IpfsEG e IpfsM where
  append e g = IpfsEG . Set.singleton <$> storeEv (Event e g)
  merge (IpfsEG g1) (IpfsEG g2) = 
    do rs1 <- refHistory (IpfsEG g1)
       rs2 <- refHistory (IpfsEG g2)
       return . IpfsEG $ Set.union (g1 Set.\\ rs2) (g2 Set.\\ rs1)
  edge (IpfsEG es) = map (\(Event e g) -> (e,g)) <$> mapM fetchEv (Set.toList es)

refHistory :: (Show e, Read e, Ord e) => IpfsEG e -> IpfsM (Set (EventRef e))
refHistory g = foldl' Set.union Set.empty 
               <$> (mapM (collectRefs . snd) =<< edge g)

collectRefs :: (Show e, Read e, Ord e) => IpfsEG e -> IpfsM (Set (EventRef e))
collectRefs (IpfsEG g1) = 
  do es <- edge (IpfsEG g1)
     foldM (\s1 (_,g2) -> Set.union s1 <$> (collectRefs g2)) g1 es
