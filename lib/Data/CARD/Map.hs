{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.CARD.Map
  ( MapE
  , mapE
  , fromKeyE
  , MapC
  , onKeyC
  , onAllC
  , fromKeyC
  ) where

import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics

import Data.CARD.Classes
import Data.CARD.Maybe


{-| Effects on a 'Data.Map.Map' containing states, lifting a domain of
    effects on the individual states. -}
data MapE k e v
  = MapE { emap :: Map k (MaybeE e v) }
  deriving (Show,Eq,Ord,Generic)

-- MapE can't simply be a type alias for Map, since Map already has a
-- Monoid instance which overwrites same-key entries rather than
-- monoidally combining them like we want to.

instance (ToJSON k, ToJSONKey k, ToJSON e, ToJSON v) => ToJSON (MapE k e v) where
  toEncoding = genericToEncoding defaultOptions
instance (Ord k, FromJSON k, FromJSONKey k, FromJSON e, FromJSON v) => FromJSON (MapE k e v)

instance (Ord k, EffectDom e v) => Semigroup (MapE k e v) where
  MapE a2 <> MapE a1 = MapE $ Map.unionWith (<>) a2 a1

instance (Ord k, EffectDom e v) => Monoid (MapE k e v) where
  mempty = MapE Map.empty

instance (Ord k, EffectDom e v) => EffectDom (MapE k e v) (Map k v) where
  eFun (MapE es) s =
    let f k (ConstE (Just v)) s = Map.insert k v s
        f k (ConstE Nothing) s = Map.delete k s
        f k (ModifyE (JustE e)) s = Map.adjust (eFun e) k s
    in Map.foldrWithKey' f s es

{-| Use a 'MaybeE' effect to set the value of a key in a map.

@
'eFun' ('mapE' k ('insertE' v)) = Data.Map.insert k v
'eFun' ('mapE' k ('adjustE' e)) = Data.Map.adjust k ('eFun' e)
'eFun' ('mapE' k 'deleteE') = Data.Map.delete k
'extractEffect' k 'Data.Function..' 'injectEffect' k = 'Data.Function.id'
@
-}
mapE :: (Ord k, Eq e, Eq v, EffectDom e v)
  => k
  -> MaybeE e v
  -> MapE k e v
mapE k e | e == mempty = MapE $ Map.empty
         | otherwise = MapE $ Map.singleton k e

{-| Extract the 'MaybeE' effect on a particular key that a 'MapE'
    effect contains.  If there is no effect on that key, this will
    return 'Data.Monoid.mempty'.

@
'fromKeyE' k (mapE k ('adjustE' e) = 'adjustE' e
'fromKeyE' k1 (mapE k2 ('adjustE' e) = 'Data.Monoid.mempty'
@
-}
fromKeyE :: (Ord k, EffectDom e v)
  => k
  -> MapE k e v
  -> MaybeE e v
fromKeyE k (MapE m) = case Map.lookup k m of
                        Just e -> e
                        Nothing -> mempty

data MapC k c
  = MapC { allKeys :: MaybeC c
         , perKey :: Map k (MaybeC c)
         }
  deriving (Show,Eq,Ord,Generic)

instance (ToJSON k, ToJSONKey k, ToJSON c) => ToJSON (MapC k c) where
  toEncoding = genericToEncoding defaultOptions
instance (Ord k, FromJSON k, FromJSONKey k, FromJSON c) => FromJSON (MapC k c)

instance (Ord k, Semigroup c) => Semigroup (MapC k c) where
  MapC a1 p1 <> MapC a2 p2 =
    MapC (a1 <> a2) $ Map.unionWith (<>) p1 p2

instance (Ord k, Monoid c) => Monoid (MapC k c) where
  mempty = MapC uniC Map.empty

instance (Ord k, Absorbing c) => Absorbing (MapC k c) where
  absorb = MapC absorb Map.empty

instance (Ord k, StateOrd c v) => StateOrd (MapC k c) (Map k v) where
  stateLe (MapC ac m) s1 s2 =
    let f k = case Map.lookup k m of
                Just c -> stateLe c (Map.lookup k s1) (Map.lookup k s2)
        f2 k = stateLe ac (Map.lookup k s1) (Map.lookup k s2)
    in and $ (f <$> Map.keys m) ++ (f2 <$> Map.keys s1 ++ Map.keys s2)

instance
  (Ord k, StateOrd c v, EffectDom e v, EffectOrd c e)
  => EffectOrd (MapC k c) (MapE k e v) where
  effectLe (MapC ac m) e1 e2 =
    let f k = case Map.lookup k m of
                Just c -> effectLe c (fromKeyE k e1) (fromKeyE k e2)
        f2 k = effectLe ac (fromKeyE k e1) (fromKeyE k e2)
    in and (f <$> Map.keys m)

onKeyC :: (Eq c, Monoid c) => k -> MaybeC c -> MapC k c
onKeyC k c | c == mempty = MapC mempty Map.empty
           | otherwise = MapC mempty $ Map.singleton k c

onAllC :: MaybeC c -> MapC k c
onAllC c = MapC c Map.empty

fromKeyC :: (Ord k, Monoid c) => k -> MapC k c -> MaybeC c
fromKeyC k (MapC b m) = case Map.lookup k m of
                          Just c -> c <> b
                          Nothing -> b
