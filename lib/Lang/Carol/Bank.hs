{-# LANGUAGE LambdaCase #-}

module Lang.Carol.Bank where

import Lang.Carol
import Data.CARD.Counter

type AccountOp = Carol (CounterC Int) (CounterE Int) Int

deposit :: Int -> AccountOp (Either String Int)
deposit n = assert (n > 0) "Must deposit at least 1." $ do
  issue (addE n)
  return (Right n)

-- | Deposit n as a reservation.
depositR :: Int -> AccountOp (Either String ())
depositR n = assert (n > 0) "Must deposit at least 1." $ do
  issue (addE n)
  produce (subE n)
  return (Right ())

withdraw :: Int -> AccountOp (Either String Int)
withdraw n = assert (n > 0) "Must withdraw at least 1." $ do
  s <- query lowerBound
  if s >= n
     then issue (subE n) >> return (Right n)
     else return (Left "Not enough in account.")

withdrawS :: Int -> AccountOp (Either String Int)
withdrawS n = withdraw n >>= \case
  Left "Not enough in account." -> do
    s <- query idC
    if s >= n
       then issue (subE n) >> return (Right n)
       else return (Left "Seriously, not enough in account.")
  other -> return other

-- | Withdraw 1 using a reservation.
withdrawR :: AccountOp Int
withdrawR = do
  let e = subE 1
  -- If the consume succeeds, we issue the same effect.  Otherwise, we
  -- issue nothing and return 0.
  consume e >>= \case
    Just _ -> issue e >> return 1
    Nothing -> return 0

current :: AccountOp Int
current = query uniC

currentS :: AccountOp Int
currentS = query idC
