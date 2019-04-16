{-# LANGUAGE TemplateHaskell #-}

module Network.Discard
  ( module Network.Discard.Broadcast
  , module Network.Discard.Crypto
  , module Network.Discard.HTTP
  , module Network.Discard.Node
  , module Network.Discard.RepCard
  ) where

import Control.Lens
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

import Data.EventGraph
import Data.EventGraph.Ipfs
import Network.Discard.Broadcast  
import Network.Discard.Crypto
import Network.Discard.HTTP
import Network.Discard.Node
import Network.Discard.RepCard

type L = Edge (IpfsEG PK)

data Discard d = Discard 
  { _feeds :: Map FeedId (ManagerConn L d)
  , _ipfsApi :: IpfsEG PK }

makeLenses ''Discard

createFeed :: d -> StateT (Discard d) IO FeedId
createFeed = undefined

loadFeeds :: FilePath -> StateT (Discard d) IO [FeedId]
loadFeeds = undefined

seekFeed :: FeedId -> StateT (Discard d) IO (Maybe d)
seekFeed = undefined

dropFeed :: FeedId -> StateT (Discard d) IO ()
dropFeed = undefined
