module Lang.CCRT
  ( CCRT
  , CCRT'
  , checkRW
  , makeRequest
  , makeCleanup
  , transactT
  , failT
  , Unsat (..)
  , CCRTFail (..)
  ) where

import Data.CARD.Classes
import Data.CARD.Capconf

-- | @'UnsatRead' c@ indicates that the read requirement, @c@, was not
-- satisfied by the local replica and there was no request strategy to
-- fix that.  @'UnsatWrite' c@ means the same for @c@ as a write-requirement.
--
-- @'WriteError' e@ means that @e@ was the completed transaction's
-- effect, but it was not covered by the transaction's
-- write-requirement and cannot be safely issued.  This indicates that
-- the transaction was not well-formed.
data CCRTFail c e = UnsatError (Unsat c) | WriteError e

data Unsat c = UnsatRead c | UnsatWrite c

data CCRT c e s m
  = CCRT { ccrtRead :: c
         , ccrtWrite :: c
         , ccrtTrans :: s -> m e
         , ccrtFail :: CCRTFail c e -> m ()
         }

data CCRT' i c e s m
  = CCRT' { ccrtRequest :: Capconf i c -> Maybe (Capconf i c)
          , ccrtCleanup :: Capconf i c -> Capconf i c
          , ccrtT :: CCRT c e s m
          }

checkRead :: (Ord i, Cap c e) => i -> CCRT' i c e s m -> Capconf i c -> Maybe e
checkRead i t cf = weaken (ccrtRead $ ccrtT t) (remoteG' i cf)

checkWrite :: (Ord i, Cap c e) => i -> CCRT' i c e s m -> Capconf i c -> Bool
checkWrite i t cf = (ccrtWrite $ ccrtT t) <=? (localG i cf)

checkRW :: (Ord i, Cap c e) => i -> CCRT' i c e s m -> Capconf i c -> Either (Unsat c) e
checkRW i t cf = if checkWrite i t cf
  then case checkRead i t cf of
    Just e -> Right e
    Nothing -> Left (UnsatRead (ccrtRead $ ccrtT t))
  else Left (UnsatWrite (ccrtWrite $ ccrtT t))

makeRequest t cf = ccrtRequest t cf

makeCleanup t cf = ccrtCleanup t cf

transactT :: CCRT' i c e s m -> s -> m e
transactT t s = (ccrtTrans $ ccrtT t) s

failT :: CCRT' i c e s m -> CCRTFail c e -> m ()
failT t = ccrtFail (ccrtT t)
