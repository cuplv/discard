module Bank where

import Data.CARD.Classes
import Data.CARD.Const
import Data.CARD.Counter
import Lang.CCRT
import Lang.CCRT.Token

type BankOp = CCRT (CounterC Int) (CounterE Int) Int
type BankOp' i = CCRT' (TokenMap i (CounterC Int)) i (CounterC Int) (CounterE Int) Int

depositT :: (Monad m) => Int -> BankOp m
depositT n =
  ccrt
    uniC -- Remote replicas can do anything.
    (addC n) -- We need to be able to add n.
    (const $ return (addE n)) -- We simply add n.

withdrawT :: (Monad m) => Int -> BankOp m
withdrawT n =
  ccrt
    lowerBound -- Remote replicas must respect a lower-bound.
    (subC n) -- We need to be able to subtract n.
    (\s -> if s >= n
              then return $ subE n
              else return $ idE)

bankOp :: (Ord i, Monad m) => i -> BankOp m -> BankOp' i m
bankOp = tokenT
