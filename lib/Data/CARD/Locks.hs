{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.CARD.Locks
  ( Locks
  -- * Create locks
  , initTokens
  , initLocks
  -- * Modify the locks
  , request
  , grant
  , release
  -- * Examine the locks
  , permitted'
  , requested
  , ungranted
  , holding
  , holding'
  , confirmed
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State
import GHC.Generics hiding (Rep)
import Data.Aeson
import Data.List (nub)
import Data.Foldable (fold,foldl')

import Control.Monad.Free

import Data.CvRDT
import Data.CARD

data Token i = Token
  { tkSeqNum :: Int
  , tkOwner :: i
  , tkState :: Maybe (Set i)
  , tkRequests :: Set i }
  deriving (Eq,Ord,Generic)

deriving instance (Show i) => Show (Token i)

nextId :: (Ord i) => i -> Set i -> Maybe i
nextId i ids =
  let ids' = Set.filter (> i) ids
  in if Set.null ids'
        then Set.lookupMin ids -- wrap-around case
        else Set.lookupMin ids'

initToken :: (Ord i) => i -> Token i
initToken i = Token 0 i (Just (Set.singleton i)) mempty

-- | Either pass token to next owner, or release lock
passToken :: (Ord i) => Token i -> Token i
passToken t@(Token n i s rs) =
  let s' = case s of
             Just ss -> Just ss
             Nothing -> Just (Set.singleton i)
  in case nextId i rs of
       Just i' -> Token (n + 1) i' s' (Set.delete i' rs)
       Nothing -> Token n i Nothing rs

requestToken :: (Ord i) => i -> Token i -> Token i
requestToken i (Token n o Nothing rs) | i == o = 
  Token (n + 1) o (Just (Set.singleton i)) rs
requestToken i (Token n o s rs) | i /= o = Token n o s (Set.insert i rs)
requestToken i t = t

instance (ToJSON i) => ToJSON (Token i)
instance (Ord i, FromJSONKey i, FromJSON i) => FromJSON (Token i)

instance (Ord i) => Semigroup (Token i) where
  t1 <> t2 | tkSeqNum t1 < tkSeqNum t2 = t2
  t1 <> t2 | tkSeqNum t1 > tkSeqNum t2 = t1
  Token n i s1 rs1 <> Token _ _ s2 rs2 = Token n i s' (rs1 <> rs2)
    where s' = case (s1,s2) of
                 (Just ss1, Just ss2) -> Just $ ss1 <> ss2
                 _ -> Nothing

data Locks i c =
    Locks { locks :: Map i (Int, c, Set i) }
  | Tokens { tokens :: Map c (Token i)}
  deriving (Show,Eq,Ord,Generic)

-- deriving instance (Show i, Show (Cr s)) => Show (Locks i s)

instance (ToJSONKey i, ToJSON i, ToJSONKey c, ToJSON c) => ToJSON (Locks i s)
instance (Ord i, FromJSONKey i, FromJSON i, Ord c, FromJSONKey c, FromJSON c) => FromJSON (Locks i s)

instance (Ord i, Semigroup c) => Semigroup (Locks i c) where
  (<>) (Locks m1) (Locks m2) =
    Locks $ foldl' (\m k -> case (Map.lookup k m1, Map.lookup k m2) of
      (Just (i1,c1,s1),Just (i2,c2,s2)) -> case compare i1 i2 of
        EQ | c1 == c2 -> Map.insert k (i1, c1, s1 `Set.union` s2) m
           | c1 `impl` c2 -> Map.insert k (i1, c1, s1) m
           | c2 `impl` c1 -> Map.insert k (i1, c2, s2) m
           | otherwise -> Map.insert k (i1, c1 <> c2, Set.empty) m
        LT -> Map.insert k (i2,c2,s2) m
        GT -> Map.insert k (i1,c1,s1) m
      (Just v1,Nothing) -> Map.insert k v1 m
      (Nothing,Just v2) -> Map.insert k v2 m) 
                   mempty (nub $ Map.keys m1 ++ Map.keys m2)
  (<>) (Tokens ts1) (Tokens ts2) =
    Tokens $ foldl' (\m k -> case (Map.lookup k ts1, Map.lookup k ts2) of
      (Just t1, Just t2) -> Map.insert k (t1 <> t2) m
      (Just t1, Nothing) -> Map.insert k t1 m
      (Nothing, Just t2) -> Map.insert k t2 m)
                    mempty (nub $ Map.keys ts1 ++ Map.keys ts2)
  (<>) (Tokens ts) _ = Tokens ts
  (<>) _ (Tokens ts) = Tokens ts

instance (Ord i) => Monoid (Locks i c) where
  mempty = Locks Map.empty

instance (Ord i, Monad m) => CvRDT r (Locks i c) m where
  cvmerge _ s1 s2 = pure (s1 <> s2)
  cvempty _ = pure mempty

-- | Create a set of tokens, one for each Conref, which all start out
-- owned by the given replica id, in an unlocked state.
initTokens :: (Ord i) => [c] -> i -> Locks i c
initTokens cs i = Tokens $ Map.fromList (zip cs (repeat $ initToken i))

-- | Create an empty set of shared locks.  This can also be
-- accomplished by using 'mempty'.
initLocks :: (Ord i) => Locks i c
initLocks = mempty

-- | Get the current request index for an identity
reqIndex :: (Ord i, c) => i -> Locks i c -> Int
reqIndex i (Locks m) = case Map.lookup i m of
                         Just (n,_,_) -> n
                         Nothing -> 0

-- | Request a new/stronger lock
request :: (Ord i, c) => i -> c -> Locks i c -> Locks i c
request i c (Tokens ts) = 
  Tokens $ foldr (Map.adjust (requestToken i)) ts c
request i c m = m <> (Locks $ Map.singleton i (reqIndex i m, c, mempty))

-- | Grant a request
grant :: (Ord i) 
      => i -- ^ Granting identity
      -> i -- ^ Identity whose request is being granted
      -> Locks i c
      -> Locks i c
grant ig ir (Tokens ts) = Tokens $ Map.map f ts
  where f (Token n o (Just ss) rs) | ir == o = 
                                     Token n o (Just (Set.insert ig ss)) rs
        f (Token n o Nothing rs) | ig == o = passToken (Token n o Nothing rs)
        f t = t
grant ig ir (Locks m) =
  Locks $ Map.adjust (\(n,c,s) -> (n,c,Set.insert ig s)

-- | Release a lock
release :: (Ord i, Monoid c) => i -> Locks i c -> Locks i c
release i (Tokens ts) = Tokens $ Map.map f ts
  where f t = if tkOwner t == i
                 then passToken t
                 else t
release i m = m <> (Locks $ Map.singleton i (reqIndex i m + 1, uniC, mempty))

-- | List all requests that 'i' has not granted
ungranted :: (Ord i) => i -> Locks i c -> [(i,c)]
ungranted i (Tokens ts) = Map.foldrWithKey f [] ts
  where f c (Token _ o (Just ss) rs) cs = if Set.member i ss
                                             then cs
                                             else (o,c) : cs
        f c (Token _ o Nothing rs) cs | o == i = case nextId i rs of
                                                   Just i' -> (i',c) : cs
                                                   Nothing -> cs
        f _ _ cs = cs
ungranted i (Locks m) = map (\(ir,(_,c,_)) -> (ir,c)) $ filter ug (Map.assocs m)
  where ug (ir,((_,c,s))) = i /= ir && not (i `Set.member` s) && c /= uniC

-- | Check if effect is blocked by current locks, returning the
-- blocking 'Conref' if so (as compared with consumed effect)
permitted' :: (Ord i)
  => i
  -> e -- ^ The consumed effect
  -> e -- ^ The issued/produced effect
  -> Locks i c
  -> Either c ()
permitted' i ce ie (Tokens ts) =
  let f (c, (Token _ o (Just ss) _)) = not (effectLe c ce ie) 
                                       && Set.member i ss
                                       && o /= i
      f _ = False
      blockers = filter f $ Map.assocs ts
  in case blockers of
       [] -> Right ()
       bs -> Left (mconcat bs)
       -- bs -> Left (fold (map (cr.fst) bs))
permitted' i ce ie ls =
  let blockers = map (\(_,c,_) -> c)
                 . filter (\(n,c,s) -> not (effectLe c ce ie)
                                       && Set.member i s)
                 . Map.elems
                 . locks
                 $ ls
  in case blockers of
       [] -> Right ()
       bs -> Left (mconcat bs)

-- | Check if lock has been requested
requested :: (Ord i, Monoid c) => i -> c -> Locks i c -> Bool
requested i c (Tokens ts) = case Map.lookup c ts of
  Just (Token _ o (Just _) _) | i == o -> True
  Just (Token _ o _ rs) | i /= o && Set.member i rs -> True
  _ -> False
requested i c1 (Locks m) = case Map.lookup i m of
  Just (_,c2,_) -> c2 `impl` c1
  Nothing -> if c1 == uniC
                then True
                else False

-- | Check if node 'i' is holding any locks
holding :: (Ord i, Monoid c) => i -> Locks i c -> Bool
holding i (Tokens ts) | holding' i (Tokens ts) == Nothing = False
holding i (Tokens ts) = True
holding i (Locks m) = case Map.lookup i m of
  Just (_,c,_) -> if c == uniC
                     then False
                     else True
  Nothing -> False

-- | Return exactly the lock that 'i' is holding
holding' :: (Ord i) => i -> Locks i c -> Maybe c
holding' i (Tokens ts) = Map.foldrWithKey f crT ts
  where f c t cs | tkState t /= Nothing && tkOwner t == i =
                   Just (c <> cs)
        f _ _ cs = Just cs
holding' i (Locks m) = case Map.lookup i m of
  Just (_,c,_) -> Just c
  Nothing -> Nothing

-- | Check if currently requested lock has been confirmed by all
-- participating identities
confirmed :: (Ord i, Monoid c)
          => i -- ^ Identity to check the requested lock of
          -> [i] -- ^ List of other participating identities
          -> Locks i c
          -> Bool
confirmed self others (Tokens ts) = Map.foldr f True ts
  where f (Token n o (Just ss) cs) rem | self == o =
                                         (Set.isSubsetOf
                                            (Set.fromList others)
                                            ss) && rem
        f _ rem = rem
confirmed self others (Locks m) =
  case Map.lookup self m of
    Just (_,c,s) ->
      c == uniC
      || Set.fromList others `Set.isSubsetOf` s
    Nothing -> True
