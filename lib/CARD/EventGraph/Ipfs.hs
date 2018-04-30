{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module CARD.EventGraph.Ipfs where

import qualified System.IO as IO (hFlush,stdout)
import Turtle
import Turtle.Prelude (cd,ls,mv)
import Control.Foldl (list)
import Data.Text (Text,pack,unpack,stripEnd)

import Data.Yaml
import Data.Aeson (pairs,foldable)
import Data.Semigroup ((<>))

import Data.Set (Set)
import qualified Data.Set as Set

import CARD.EventGraph

-- | An IPFS object
newtype IObject = IObject Text

newtype IpfsEG e = IpfsEG (Set IObject)

instance ToJSON (IpfsEG e) where
  toJSON (IpfsEG is) = object (Set.toList is)

instance (Ord e) => EventGraph IpfsEG e where
  empty = IpfsEG Set.empty
  
data Event e = Event e (IpfsEG e)



instance Event

instance (Show e, Read e, Ord e) => MonadEG IpfsEG e IO where
  append e (IpfsEG) = undefined
  merge = undefined
  edge = undefined
  
