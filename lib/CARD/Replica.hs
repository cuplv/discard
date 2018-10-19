{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module CARD.Replica where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Concurrent.STM hiding (check)
import Control.Monad.State

import CARD.EventGraph
import CARD.Store
import CARD.Network

data RepConfig i r s t = RepConfig 
  { selfId :: i
  , selfAddr :: Src t
  , msgs :: TChan (CMsg i r s)
  , initStore :: s
  , graphResolver :: r
  , others :: Map i (Dest t) }

data RepRT i r s t = RepRT
  { repConfig :: RepConfig i r s t
  , initVal :: s
  , repHist :: Hist i r s }

class (Eq i, Ord i, Transport t, Store s, Carries t i r s, EG r (i, Effect s) (Res t), Res t ~ IO) => Rep i r s t

instance (Eq i, Ord i, Transport t, Store s, Carries t i r s, EG r (i, Effect s) (Res t), Res t ~ IO) => Rep i r s t

type RepS i r s t = StateT (RepRT i r s t) (Res t)

bcast' :: (Rep i r s t) => RepConfig i r s t -> Hist i r s -> Res t ()
bcast' (RepConfig i _ _ _ _ others) hist = do
  let msg = BCast i hist
  Map.foldl' (\m dest -> m >> send dest msg) (return ()) others

bcast :: (Rep i r s t) => RepS i r s t ()
bcast = do (RepRT conf _ hist) <- get
           lift$ bcast' conf hist

emit' :: (Rep i r s t) => RepRT i r s t -> Effect s -> Res t (RepRT i r s t)
emit' (RepRT conf iv hist) e = do
  hist' <- appendAs (selfId conf) (graphResolver conf) e hist
  bcast' conf hist'
  return (RepRT conf iv hist')

emit :: (Rep i r s t) => Effect s -> RepS i r s t (Hist i r s)
emit e = do rep <- get
            rep' <- lift$ emit' rep e
            put rep'
            return (repHist rep')

update' :: (Rep i r s t) => Bool -> RepRT i r s t -> Res t (RepRT i r s t)
update' wait (RepRT conf iv hist1) = do
  let listen = do msg <- atomically $ readTChan (msgs conf)
                  case msg of
                    BCast _ h -> ([h] ++) <$> listenMore
                    _ -> listen
      listenMore = do mym <- atomically $ tryReadTChan (msgs conf)
                      case mym of
                        Just (BCast _ h) -> ([h] ++) <$> listenMore
                        Just _ -> listenMore
                        Nothing -> return []
  newHists <- if wait
                 then listen
                 else listenMore
  hist2 <- foldM (merge (graphResolver conf)) hist1 newHists
  return $ RepRT conf iv hist2

check :: (Rep i r s t) => RepS i r s t s
check = do checkH
           (RepRT conf iv hist) <- get
           lift$ evalHist (graphResolver conf) iv hist

-- | Merge in the next broadcast from another replica and return the
-- updated state.  If more than one unseen broadcasts have arrived,
-- they will all be merged.
update :: (Rep i r s t) => RepS i r s t s
update = do updateH
            (RepRT conf iv hist) <- get
            lift$ evalHist (graphResolver conf) iv hist

checkH :: (Rep i r s t) => RepS i r s t (Hist i r s)
checkH = do rep <- get
            rep' <- lift$ update' False rep
            put rep'
            return (repHist rep')

updateH :: (Rep i r s t) => RepS i r s t (Hist i r s)
updateH = do rep <- get
             rep' <- lift$ update' True rep
             put rep'
             return (repHist rep')

await :: (Rep i r s t) => (s -> Bool) -> RepS i r s t s
await p = do s <- check
             if p s
                then return s
                else await p

await' :: (Rep i r s t) => (s -> Bool) -> RepS i r s t s
await' p = do s <- update
              if p s
                 then return s
                 else await' p
