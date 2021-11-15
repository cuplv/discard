module Lang.CCRT
  ( CCRT
  , CCRT'
  , checkRW
  , makeRequest
  , makeCleanup
  , transactT
  , failT
  ) where

import Data.CARD.Classes
import Data.CARD.Capconf

data CCRT c e s m
  = CCRT { ccrtRead :: c
         , ccrtWrite :: c
         , ccrtTrans :: s -> m e
         , ccrtFail :: m ()
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

checkRW :: (Ord i, Cap c e) => i -> CCRT' i c e s m -> Capconf i c -> Maybe e
checkRW i t cf = if checkWrite i t cf
                    then checkRead i t cf
                    else Nothing

makeRequest t cf = ccrtRequest t cf

makeCleanup t cf = ccrtCleanup t cf

transactT :: CCRT' i c e s m -> s -> m e
transactT t s = (ccrtTrans $ ccrtT t) s

failT :: CCRT' i c e s m -> m ()
failT = ccrtFail . ccrtT
