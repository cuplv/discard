{-# LANGUAGE LambdaCase #-}

module CARD.LQ.Bank where

import CARD

deposit :: Int -> Op Counter (Either String Int)
deposit n = assert (n > 0) "Must deposit at least 1." $ do
  issue (ef$ Add n)
  return (Right n)

withdraw :: Int -> Op Counter (Either String Int)
withdraw n = assert (n > 0) "Must withdraw at least 1." $ do
  (Counter s) <- query (cr$ LEQ)
  if s >= n
     then issue (ef$ Sub n) >> return (Right n)
     else return (Left "Not enough in account.")

withdrawS :: Int -> Op Counter (Either String Int)
withdrawS n = withdraw n >>= \case
  Left "Not enough in account." -> do
    (Counter s) <- query crEqv
    if s >= n
       then issue (ef$ Sub n) >> return (Right n)
       else return (Left "Seriously, not enough in account.")
  other -> return other

current :: Op Counter Int
current = do (Counter s) <- query crT
             return s

currentS :: Op Counter Int
currentS = do (Counter s) <- query crEqv
              return s
