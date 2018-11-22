module Lang.Carol 
  ( LQ
  , HelpMe (..)
  , helpMe
  , runLQ
  , runLQ'
  , issue
  , query
  , assert
  , module Data.CARD

  ) where

import Lang.Carol.Internal
import Data.CARD

assert :: Bool -- ^ Assert condition
       -> String -- ^ Failure message
       -> LQ s (Either String a) -- ^ Remaining term
       -> LQ s (Either String a)
assert b s op = if b
                   then op
                   else return (Left s)
