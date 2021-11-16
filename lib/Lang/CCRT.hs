module Lang.CCRT
  ( CCRT (..)
  , CCRT' (..)
  , ReqHandler
  , ccrt
  , emptyReqHandler
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
         , ccrtFail :: CCRTFail c e -> m ()
         , ccrtTrans :: s -> m e
         }

ccrt :: (Monad m) => c -> c -> (s -> m e) -> CCRT c e s m
ccrt r w f = CCRT r w (const $ return ()) f

data CCRT' q i c e s m
  = CCRT' { ccrtRequest :: q -> Maybe q
          , ccrtCleanup :: ReqHandler q i c
          , ccrtT :: CCRT c e s m
          }

type ReqHandler q i c = q -> Capconf i c -> Capconf i c

emptyReqHandler = \_ -> id

checkRead :: (Ord i, Cap c e) => i -> CCRT' q i c e s m -> Capconf i c -> Maybe e
checkRead i t cf = weaken (ccrtRead $ ccrtT t) (remoteG' i cf)

checkWrite :: (Ord i, Cap c e) => i -> CCRT' q i c e s m -> Capconf i c -> Bool
checkWrite i t cf = (ccrtWrite $ ccrtT t) <=? (localG i cf)

checkRW :: (Ord i, Cap c e) => i -> CCRT' q i c e s m -> Capconf i c -> Either (Unsat c) e
checkRW i t cf = if checkWrite i t cf
  then case checkRead i t cf of
    Just e -> Right e
    Nothing -> Left (UnsatRead (ccrtRead $ ccrtT t))
  else Left (UnsatWrite (ccrtWrite $ ccrtT t))

makeRequest :: CCRT' q i c e s m -> q -> Maybe q
makeRequest t q = ccrtRequest t q

makeCleanup :: CCRT' q i c e s m -> q -> Capconf i c -> Capconf i c
makeCleanup t q cf = ccrtCleanup t q cf

transactT :: CCRT' q i c e s m -> s -> m e
transactT t s = (ccrtTrans $ ccrtT t) s

failT :: CCRT' q i c e s m -> CCRTFail c e -> m ()
failT t = ccrtFail (ccrtT t)
