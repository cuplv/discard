{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

-- | 
-- Module: CARD.Store.CA
-- 
-- This module provides store-level support for conflict avoidance.
-- Replicas hosting the 'CA i s' datatype can use the store itself to
-- request and grant locks, meaning that the network only needs to
-- provide update broadcasts.

module CARD.Store.CA where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State
import GHC.Generics hiding (Rep)
import Data.Aeson

import CARD.Store
import CARD.Replica
import CARD.Network

type Locks i s = Map i (Conref s, Set i)

data CA i s = CA 
  { locks :: Locks i s
  , caStore :: s } 
  deriving (Eq,Ord,Generic)

instance (Store s, Eq i, Ord i) => Store (CA i s) where
  data Ef (CA i s) = Request (i, Conref s) 
                   | Grant i i 
                   | Release i
                   | Lift (Effect s)
                   deriving (Eq,Ord,Generic)
  data Cr (CA i s) = CrCA deriving (Eq,Ord,Generic)

  defineEffect (CA ls s) e = case e of
    Lift e' -> CA ls (runEffect s e')
    Request (ir,c1) -> case Map.lookup ir ls of
      Just (c2,_) -> CA (Map.insert ir (c1 |&| c2, Set.empty) ls) s
      Nothing -> CA (Map.insert ir (c1, Set.empty) ls) s
    Grant ig ir -> 
      let addG ig = Map.adjust (\(c,gs) -> (c,Set.insert ig gs))
      in CA (addG ig ir ls) s
    Release i -> CA (Map.delete i ls) s

  defineConflict _ _ = False

instance (ToJSONKey i, ToJSON i, ToJSON s, ToJSON (Cr s)) => ToJSON (CA i s)
instance (Ord i, FromJSONKey i, FromJSON i, FromJSON s, Ord (Cr s), FromJSON (Cr s)) => FromJSON (CA i s)
instance (ToJSON i, ToJSON (Cr s), ToJSON (Ef s)) => ToJSON (Ef (CA i s))
instance (FromJSON i, FromJSON (Cr s), FromJSON (Ef s), Ord (Ef s), Ord (Cr s)) => FromJSON (Ef (CA i s))
instance ToJSON (Cr (CA i s))
instance FromJSON (Cr (CA i s))

initCA :: s -> CA i s
initCA s = CA (Map.empty) s

permitted :: (Ord i, Store s) => i -> Effect s -> CA i s -> Bool
permitted i e =
  not
  . or 
  . map (\(c,is) -> checkBlock c e && Set.member i is)
  . Map.elems
  . locks

confirmed :: (Ord i) => i -> [i] -> CA i s -> Bool
confirmed self others (CA ls _) = 
  case Map.lookup self ls of
    Just (_,cs) -> Set.fromList others `Set.isSubsetOf` cs
    Nothing -> True


query :: (Store s, Rep i r (CA i s) t) => Conref s -> RepS i r (CA i s) t s
query c = do conf <- repConfig <$> get
             let i = selfId conf
                 os = Map.keys (others conf)
             if c /= Conref (Set.empty)
                then do emit (ef$ Request (i,c))
                        caStore <$> await (confirmed i os)
                else caStore <$> check



release :: (Rep i r (CA i s) t) => RepS i r (CA i s) t ()
release = do i <- selfId . repConfig <$> get
             (CA ls _) <- check
             if i `Map.member` ls
                then emit (ef$ Release i) >> lift (putStrLn ("Released."))
                else return ()

safeEmit :: (Store s, Rep i r (CA i s) t) => Effect s -> RepS i r (CA i s) t ()
safeEmit e = do i <- selfId . repConfig <$> get
                await (permitted i e) 
                emit (ef$ Lift e)
                return ()

grantAll :: (Show i, Show (Cr s), Rep i r (CA i s) t) => RepS i r (CA i s) t ()
grantAll = do i <- selfId . repConfig <$> get
              (CA ls _) <- check
              let unGranted = 
                    filter (\(ir,(_,igs)) -> i `Set.notMember` igs) 
                    . Map.assocs 
                    $ ls
              mapM_ (\(ir,l) -> emit (ef$ Grant i ir) >> lift (putStrLn (show (ir,l)))) unGranted
