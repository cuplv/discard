module Lang.CCRT
  ( CCRT (..)
  , CCRT' (..)
  , CCRTR
  , ReqHandler
  , Runner
  , runT
  , runTR
  , ccrt
  , ccrtR
  , emptyReqHandler
  , checkRW
  , makeRequest
  , makeCleanup
  , transactT
  , failT
  , Unsat (..)
  , CCRTFail (..)
  , noHandle
  ) where

import Data.CARD.Classes
import Data.CARD.Capconf

import Control.Concurrent.STM

-- | @'UnsatRead' c@ indicates that the read requirement, @c@, was not
-- satisfied by the local replica and there was no request strategy to
-- fix that.  @'UnsatWrite' c@ means the same for @c@ as a write-requirement.
--
-- @'WriteError' e@ means that @e@ was the completed transaction's
-- effect, but it was not covered by the transaction's
-- write-requirement and cannot be safely issued.  This indicates that
-- the transaction was not well-formed.
data CCRTFail c e
  = UnsatError (Unsat c)
  | WriteError e
  deriving (Show,Eq,Ord)

data Unsat c = UnsatRead c c | UnsatWrite c c deriving (Show,Eq,Ord)

data CCRT c e s m
  = CCRT { ccrtRead :: c
         , ccrtWrite :: c
         , ccrtFail :: CCRTFail c e -> m ()
         , ccrtTrans :: s -> m e
         }

ccrt :: (Monad m) => c -> c -> (s -> m e) -> CCRT c e s m
ccrt r w f = CCRT r w (const $ return ()) f

type CCRTR c e s a = TMVar (Either (CCRTFail c e) a) -> CCRT c e s IO

ccrtR :: c -> c -> (s -> IO (e,a)) -> TMVar (Either (CCRTFail c e) a) -> CCRT c e s IO
ccrtR r w f tmv =
  CCRT
    r
    w
    (\z -> atomically (putTMVar tmv (Left z)) >> return ())
    (\s -> do (e,a) <- f s
              atomically (putTMVar tmv (Right a))
              return e)

data CCRT' q i c e s m
  = CCRT' { ccrtRequest :: q -> Maybe q
          , ccrtCleanup :: ReqHandler q i c
          , ccrtT :: CCRT c e s m
          }

type ReqHandler q i c = (q, Capconf i c) -> (q, Capconf i c)

emptyReqHandler = id

checkRead :: (Ord i, Cap c e) => i -> CCRT' q i c e s m -> Capconf i c -> Maybe e
checkRead i t cf = weaken (ccrtRead $ ccrtT t) (remoteG' i cf)

checkWrite :: (Ord i, Cap c e) => i -> CCRT' q i c e s m -> Capconf i c -> Bool
checkWrite i t cf = (ccrtWrite $ ccrtT t) <=? (localG i cf)

checkRW :: (Ord i, Cap c e) => i -> CCRT' q i c e s m -> Capconf i c -> Either (Unsat c) e
checkRW i t cf = if checkWrite i t cf
  then case checkRead i t cf of
    Just e -> Right e
    Nothing -> Left (UnsatRead (ccrtRead $ ccrtT t) (remoteG' i cf))
  else Left (UnsatWrite (ccrtWrite $ ccrtT t) (localG i cf))

makeRequest :: CCRT' q i c e s m -> q -> Maybe q
makeRequest t q = ccrtRequest t q

makeCleanup :: CCRT' q i c e s m -> ReqHandler q i c
makeCleanup t a = ccrtCleanup t a

transactT :: CCRT' q i c e s m -> s -> m e
transactT t s = (ccrtTrans $ ccrtT t) s

failT :: CCRT' q i c e s m -> CCRTFail c e -> m ()
failT t = ccrtFail (ccrtT t)

type Runner q i c e s m = CCRT' q i c e s m -> m ()

runT
  :: (Monad m)
  => Runner q i c e s m
  -> (CCRT c e s m -> CCRT' q i c e s m)
  -> CCRT c e s m
  -> m ()
runT r h t = r (h t)

runTR
  :: Runner q i c e s IO
  -> (CCRT c e s IO -> CCRT' q i c e s IO)
  -> (TMVar (Either (CCRTFail c e) a) -> CCRT c e s IO)
  -> IO (Either (CCRTFail c e) a)
runTR r h f = do
  tmv <- newEmptyTMVarIO
  let t = f tmv
  r (h t)
  atomically (takeTMVar tmv)

noHandle :: CCRT c e s m -> CCRT' q i c e s m
noHandle = CCRT' (const Nothing) id
