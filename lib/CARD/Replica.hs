module CARD.Replica where

import Data.Map (Map)

import CARD.Store
import CARD.Network

data RepConfig i r s t = RepConfig 
  { selfId :: i
  , selfAddr :: Src t
  , initStore :: s
  , others :: Map i (Dest t) }

data RepRT i r s t = RepRT
  { repConfig :: RepConfig i r s t
  , repLocks :: Map i (Conref s)
  , repHist :: Hist i r s }
