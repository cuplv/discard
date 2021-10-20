{-# LANGUAGE LambdaCase #-}

module Lang.Carol.Warehouse
  ( -- * Using reservations and queries
    sellR
  , restockQR
    -- * Using queries only
  , sellQ
  , restockQQ
  ) where

import Lang.Carol
import Data.CARD.Counter

type WarehouseOp = Carol (CounterC Int) (CounterE Int) Int

-- | Sell a unit, using a reservation for safety.
sellR :: WarehouseOp Bool
sellR = do
  let e = subE 1
  consume e >>= \case
    Just _ -> do issue e
                 return True
    Nothing -> return False

-- | Sell a unit, using a query for safety.
sellQ :: WarehouseOp Bool
sellQ = do
  s <- query lowerBound
  if s >= 1
     then issue (subE 1) >> return True
     else return False

-- | Restock, producing reservations.
restockQR :: Int -> WarehouseOp Int
restockQR maxSpace = do
  n <- restockQQ maxSpace
  if n > 0
     then produce (subE n) >> return n
     else return 0

-- | Restock without producing reservations (so that units can be
-- claimed using queries).
restockQQ :: Int -> WarehouseOp Int
restockQQ maxSpace = do
  s <- query upperBound
  let n = maxSpace - s
  if n > 0
     then issue (addE n) >> return n
     else return 0
