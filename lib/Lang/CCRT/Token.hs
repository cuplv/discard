{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lang.CCRT.Token where

import Data.CvRDT
import Data.CARD.Classes
import Data.CARD.Capconf
import Lang.CCRT

import Data.Aeson
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)

data Token i
  = Token { tkSeqNum :: Int
          , tkOwner :: i
          , tkRequests :: Set i
          }
  deriving (Show,Eq,Ord,Generic)

instance (Ord i, ToJSON i) => ToJSON (Token i)
instance (Ord i, FromJSON i) => FromJSON (Token i)

instance (Ord i) => Semigroup (Token i) where
  t1 <> t2 | tkSeqNum t1 < tkSeqNum t2 = t2
           | tkSeqNum t1 > tkSeqNum t2 = t1
  Token n i rs1 <> Token _ _ rs2 = Token n i (Set.union rs1 rs2)

initToken :: i -> Token i
initToken i = Token 0 i Set.empty

nextId :: (Ord i) => i -> Set i -> Maybe i
nextId i ids =
  let ids' = Set.filter (> i) ids
  in if Set.null ids'
        then Set.lookupMin ids -- wrap-around case
        else Set.lookupMin ids'

passToken :: (Ord i) => Token i -> (Token i, Maybe i)
passToken t@(Token n i rs) =
  case nextId i rs of
    Just i' -> (Token (n + 1) i' (Set.delete i' rs), Just i')
    Nothing -> (t, Nothing)

requestToken :: (Ord i) => i -> Token i -> Token i
requestToken i t@(Token n o rs) | i == o = t
                                | otherwise = Token n o (Set.insert i rs)

data TokenMap i c
  = TokenMap { tokens :: Map c (Token i) }
  deriving (Show,Eq,Ord,Generic)

instance (Ord i, Ord c, ToJSON i, ToJSON c, ToJSONKey c) => ToJSON (TokenMap i c)
instance (Ord i, Ord c, FromJSON i, FromJSON c, FromJSONKey c) => FromJSON (TokenMap i c)

instance (Ord i, Ord c) => Semigroup (TokenMap i c) where
  TokenMap m1 <> TokenMap m2 =
    TokenMap $ Map.unionWith (<>) m1 m2

instance (Ord i, Ord c) => Monoid (TokenMap i c) where
  mempty = TokenMap Map.empty

instance (Ord i, Ord c, Monad m) => CvRDT r (TokenMap i c) m where
  cvempty _ = return $ mempty
  cvmerge _ q1 q2 = return $ q1 <> q2

fromJust (Just a) = a
fromJust Nothing = error "fromJust got Nothing."

tokenReqHandler
  :: (Ord i, Ord c, Meet c, Monoid c, Split c)
  => i
  -> ReqHandler (TokenMap i c) i c
tokenReqHandler i (TokenMap m, cf) =
  let f c t (m,l) | tkOwner t == i = case passToken t of
                      (t',Just i') -> (Map.insert c t' m, (c,i'):l)
                      (t,Nothing) -> (Map.insert c t m, l)
                  | otherwise = (Map.insert c t m,l)
      (m',l') = Map.foldrWithKey f (Map.empty,[]) m

      f2 (c,i') = maskG i (i',c) . unmaskAllG' i c
  in (TokenMap m', foldr f2 cf l')

tokenRequester
  :: (Ord i, Ord c, Meet c, Monoid c)
  => i
  -> c -- Read req
  -> TokenMap i c
  -> Maybe (TokenMap i c)
tokenRequester i cr (TokenMap m) =
  case List.find (\c' -> cr <=? c') (Map.keys m) of
    Just c' -> case Map.lookup c' m of
      Just t -> Just . TokenMap $ (Map.insert c' (requestToken i t) m)
      Nothing -> Nothing
    Nothing -> Nothing

tokenT :: (Ord i, Ord c, Cap c e) => i -> CCRT c e s m -> CCRT' (TokenMap i c) i c e s m
tokenT i t =
  let c = ccrtRead t
  in CCRT'
       (tokenRequester i (ccrtRead t))
       (tokenReqHandler i)
       t

initTokenMap :: (Ord c) => [(i,c)] -> TokenMap i c
initTokenMap is = TokenMap $
  foldr (\(i,c) m -> Map.insert c (initToken i) m) Map.empty is
