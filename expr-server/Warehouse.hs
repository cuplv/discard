{-# LANGUAGE FlexibleContexts #-}

module Warehouse where

import Data.CARD.Capconf
import Data.CARD.Classes
import Data.CARD.Const
import Data.CARD.Counter
import Lang.CCRT
import Lang.CCRT.Token

import Control.Concurrent.STM
import Data.Maybe (fromJust)

type Op = CCRT (CounterC Int) (CounterE Int) Int
type Op' = CCRT' (TokenMap String (CounterC Int)) String (CounterC Int) (CounterE Int) Int

sellT :: Int -> TVar Int -> Op IO
sellT amt sales =
  ccrt
    lowerBound
    (subC amt)
    (\s -> if s >= amt
              then do atomically (modifyTVar' sales (+ amt))
                      return (subE amt)
              else return idE)

restockT :: (Monad m) => Int -> Op m
restockT maxQ =
  ccrt
    upperBound
    (addC maxQ)
    (\s -> return (addE (maxQ - s)))

tmOp :: (Monad m) => String -> Op m -> Op' m
tmOp i o = (tokenT i o) { ccrtCleanup = f }
  where f e (q,cf) = (q,fromJust $ dropG i (undo e) cf)

tmHandler :: String -> ReqHandler (TokenMap String (CounterC Int)) String (CounterC Int)
tmHandler = tokenReqHandler

tmCapconf :: String -> [String] -> Capconf String (CounterC Int)
tmCapconf i is =
  foldr (\i' -> maskG i' (i,lowerBound) . maskG i' (i,upperBound))
        (mkUniform uniC (i:is)) is

tmTokens :: String -> TokenMap String (CounterC Int)
tmTokens i = initTokenMap [(i,lowerBound), (i,upperBound)]

prOp :: (Monad m) => String -> [String] -> String -> Op m -> Op' m
prOp prim sec i o | i == prim = (tokenT i o) { ccrtCleanup = f }
  where f e (q,cf) | isAddE e = let fl (s,e) cf = fromJust $ transferG i (s,undo e) cf
                                in (q, foldr fl cf (divup sec e))
                   | otherwise = (q,cf)
prOp prim sec i o = (tokenT i o) { ccrtCleanup = f }
  where f e (q,cf) | isSubE e = (q,fromJust $ transferG i (prim,undo e) cf)
                   | otherwise = (q,cf)

prInitTokens :: String -> TokenMap i (CounterC Int)
prInitTokens i = initTokenMap []

prInitCapconf :: (Ord i) => i -> [i] -> Capconf i (CounterC Int)
prInitCapconf i is = mkUniform lowerBound [i] <> mkUniform idC is
