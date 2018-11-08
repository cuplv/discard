{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module CARD.Experiment
  ( defaultServerPort
  , ExpConf (..)
  , ExpNetConf (..)
  , getNetConf
  , expAddrs
  , Mix (..)
  , Profile
  , chooseOp
  , ExpCmd (..)
  , ExpResult (..)

  ) where

import GHC.Generics
import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (foldM)

import CARD

data ExpConf = ExpConf 
  { expRate :: Int
  , expApp :: String
  , expMix :: Mix
  , expTime :: Int }
  deriving (Eq,Ord,Show,Generic)

instance ToJSON ExpConf where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON ExpConf

data Mix = Mix [(Int,String)] String deriving (Eq,Ord,Show,Generic)

instance ToJSON Mix where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Mix

type Profile s = String -> LQ s ()

chooseOp :: Profile s -> Mix -> Int -> LQ s ()
chooseOp prf (Mix ns df) n = 
  let opStr = foldr (\(n',s') s -> if n <= n'
                                      then s'
                                      else s) df ns
  in prf opStr

data ExpCmd i = Launch ExpConf (NetConf i)
              | Report Int
              deriving (Eq,Ord,Show,Generic)

instance (ToJSON i) => ToJSON (ExpCmd i) where
  toEncoding = genericToEncoding defaultOptions
instance (Ord i, FromJSON i) => FromJSON (ExpCmd i)

data ExpResult = ExpResult { expResultAvg :: Double } deriving (Show,Generic)

instance ToJSON ExpResult where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON ExpResult

data ExpNetConf i = ExpNetConf (Map i (String, Int, Int)) deriving (Show,Eq,Ord)

getNetConf :: ExpNetConf i -> NetConf i
getNetConf (ExpNetConf m) = NetConf (fmap (\(n,p,_) -> (n,p)) m)

expAddrs :: ExpNetConf i -> Map i String
expAddrs (ExpNetConf m) = fmap (\(h,_,p) -> "http://" ++ h ++ ":" ++ show p) m

instance (ToJSON i) => ToJSON (ExpNetConf i) where
  toJSON (ExpNetConf m) = 
    toJSON (map ent $ Map.assocs m)
    where ent (i,(h,p,c)) = object ["name" .= i
                                   ,"host" .= h
                                   ,"port" .= p
                                   ,"expPort" .= c]

instance (Ord i, FromJSON i) => FromJSON (ExpNetConf i) where
  parseJSON = withArray "ExpNodeList" $ \v -> ExpNetConf <$> do
    foldM unpack Map.empty v
    where unpack m = withObject "Node" $ \v -> do
            name <- v .: "name"
            host <- v .:? "host" .!= "localhost"
            port <- v .:? "port" .!= defaultPort
            expPort <- v .:? "expPort" .!= defaultServerPort
            return (Map.insert name (host,port,expPort) m)

defaultServerPort = 24001 :: Int
