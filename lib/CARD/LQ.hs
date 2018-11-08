module CARD.LQ 
  ( LQ
  , runLQ
  , issue
  , query
  , assert

  ) where

import CARD.LQ.Internal

assert :: Bool -- ^ Assert condition
       -> String -- ^ Failure message
       -> LQ s (Either String a) -- ^ Remaining term
       -> LQ s (Either String a)
assert b s op = if b
                   then op
                   else return (Left s)
