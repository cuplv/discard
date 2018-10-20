module CARD.LQ.Bank where

import CARD.Store
import CARD.LQ

deposit :: Int -> Op Counter (Either String Int)
deposit n = do assert (n > 0) "Must deposit at least 1."
               issue (ef$ Add n)
               return (Right n)

withdraw :: Int -> Op Counter (Either String Int)
withdraw n = do assert (n > 0) "Must withdraw at least 1."
                (Counter s) <- query (cr$ LEQ)
                if s >= n
                   then do issue (ef$ Sub n)
                           return (Right n)
                   else return (Left "Not enough in account.")

withdrawS :: Int -> Op Counter (Either String Int)
withdrawS n = do 
  r <- withdraw n
  case r of
    Left "Not enough in account." -> do
      (Counter s) <- query EQV
      if s >= n
         then do issue (ef$ Sub n)
                 return (Right n)
         else return (Left "Not enough in total account.")
    other -> return other

current :: Op Counter Int
current = do (Counter s) <- query crT
             return s

currentS :: Op Counter Int
currentS = do (Counter s) <- query EQV
              return s
