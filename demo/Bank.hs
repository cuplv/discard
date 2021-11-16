module Bank where

import Data.CARD.Capconf
import Data.CARD.Classes
import Data.CARD.Const
import Data.CARD.Counter
import Lang.CCRT
import Lang.CCRT.Token

import Data.Maybe (fromJust)

type BankOp = CCRT (CounterC Int) (CounterE Int) Int
type BankOp' i = CCRT' (TokenMap i (CounterC Int)) i (CounterC Int) (CounterE Int) Int

depositT :: (Monad m) => Int -> BankOp m
depositT n =
  ccrt
    uniC -- Remote replicas can do anything.
    (addC n) -- We need to be able to add n.
    (const $ return (addE n)) -- We simply add n.

depositTR n =
  ccrtR
    uniC
    (addC n)
    (const $ return (addE n,n))

withdrawT :: (Monad m) => Int -> BankOp m
withdrawT n =
  ccrt
    lowerBound
    (subC n)
    (\s -> if s >= n
              then return (subE n)
              else return idE)

-- withdrawTR :: (Monad m) => Int -> BankOp m
withdrawTR n =
  ccrtR
    lowerBound -- Remote replicas must respect a lower-bound.
    (subC n) -- We need to be able to subtract n.
    (\s -> if s >= n
              then return (subE n,n)
              else return (idE,0))

balanceT :: CCRTR (CounterC Int) (CounterE Int) Int Int
balanceT = ccrtR uniC idC (\s -> return (idE,s))

bankOp :: (Ord i, Monad m) => i -> BankOp m -> BankOp' i m
bankOp i o = (tokenT i o) { ccrtCleanup = f }
  where f e (q,cf) = (q,fromJust $ dropG i (undo e) cf)

initCapconf :: (Ord i) => i -> [i] -> Capconf i (CounterC Int)
initCapconf i is = 
  foldr (\i' -> maskG i' (i,lowerBound)) (mkUniform uniC (i:is)) is
  -- mkUniform uniC (i:is)

initTokens :: i -> TokenMap i (CounterC Int)
initTokens i = initTokenMap [(i,lowerBound)]
