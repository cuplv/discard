module Lang.Carol 
  ( Carol
  , HelpMe (..)
  , helpMe
  , runCarol
  , runCarol'
  , issue
  , query
  , assert
  , module Data.CARD

  ) where

import Lang.Carol.Internal
import Data.CARD

assert :: Bool -- ^ Assert condition
       -> String -- ^ Failure message
       -> Carol s (Either String a) -- ^ Remaining term
       -> Carol s (Either String a)
assert b s op = if b
                   then op
                   else return (Left s)
