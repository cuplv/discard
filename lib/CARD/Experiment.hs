{-# LANGUAGE DeriveGeneric #-}

module CARD.Experiment
  ( ExpConf (..)
  , Mix (..)
  , Profile
  , chooseOp
  , ExpCmd (..)

  ) where

import GHC.Generics
import Data.Aeson

import CARD

data ExpConf = ExpConf 
  { expRate :: Int
  , expApp :: String
  , expMix :: Mix }
  deriving (Eq,Ord,Show,Generic)

instance ToJSON ExpConf where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON ExpConf

data Mix = Mix [(Int,String)] String deriving (Eq,Ord,Show,Generic)

instance ToJSON Mix where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Mix

type Profile s = String -> Op s ()

chooseOp :: Profile s -> Mix -> Int -> Op s ()
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
