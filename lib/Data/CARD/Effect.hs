{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.CARD.Effect
  (
  -- * The effect domain class
    EffectDom (..)
  , idEffect
  -- * Concrete effect domains
  -- ** 'ECounter'
  , ECounter
  , addAmt
  , mulAmt
  , addE
  , subE
  , mulE
  , additive
  -- ** 'EMaybe'
  , EMaybe
  , insertE
  , adjustE
  , deleteE
  -- ** 'EMap'
  , EMap
  , mapE
  , fromKeyE
  -- * Effect domain combinators
  , EIdentity
  , EConst (..)
  , EOnFunctor (..)
  , EProduct
  , EProduct3
  , EProduct4
  ) where

import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics
import Lens.Micro.TH

{-| An 'EffectDom' is a domain of effects (@e@) on some state type
    (@s@), in which each effect denotes (by 'runEffect') a pure
    function on a state value.  The effects must form a
    'Data.Monoid.Monoid' according to the following laws:

@
\-\- Identity
'runEffect' 'Data.Monoid.mempty' = 'Data.Function.id'

\-\- Composition
e2 'Data.Semigroup.<>' e1 = 'runEffect' e2 'Data.Function..' 'runEffect' e1
@

    Note that 'Data.Semigroup.<>' composes effects right-to-left, just
    like function composition.
-}

class (Monoid e) => EffectDom e s where
  runEffect :: e -> s -> s

{-| The identity effect, a synonym for 'Data.Monoid.mempty'. -}
idEffect :: (Monoid e) => e
idEffect = mempty

{-| 'EIdentity', a synonym for @()@, is used as a trivial effect domain
    containing only the identity effect.

@
'runEffect' () = 'Data.Function.id'
@
-}
type EIdentity = ()

instance EffectDom () s where
  runEffect () = id

{-| t'EConst' wraps an effect domain (@e@) with an v'EConst' effect that
    replaces the current state with a given value.

@
'runEffect' ('EConst' s) = 'Data.Function.const' s = (\\_ -> s)
@

    Effects from the wrapped domain can be used with 'EModify'.

@
'runEffect' ('EModify' e) s = 'runEffect' e s
@
-} 
data EConst e s
  = EConst s
  | EModify e
  deriving (Show,Eq,Ord,Generic)

instance (EffectDom e s) => Semigroup (EConst e s) where
  EConst s <> _ = EConst s
  EModify e2 <> EModify e1 = EModify (e2 <> e1)
  EModify e <> EConst s = EConst (runEffect e s)

instance (EffectDom e s) => Monoid (EConst e s) where
  mempty = EModify mempty

instance (EffectDom e s) => EffectDom (EConst e s) s where
  runEffect (EConst s) = const s
  runEffect (EModify e) = runEffect e

{-| Lift an effect domain over a functor.

@
'runEffect' ('EOnFunctor' e) s = 'Data.Functor.fmap' ('runEffect' e) s
@
-}
newtype EOnFunctor e
  = EOnFunctor e
  deriving (Show,Eq,Ord,Semigroup,Monoid,Generic)

instance (Functor m, EffectDom e s) => EffectDom (EOnFunctor e) (m s) where
  runEffect (EOnFunctor e) = fmap (runEffect e)

{-| An effect domain @(e1,e2)@ applies pairs of effects to pairs of
    states.

@
'runEffect' (e1,e2) (s1,s2) = ('runEffect' e1 s1, 'runEffect' e2 s2)
@
-}
type EProduct e1 e2 = (e1,e2)

type EProduct3 e1 e2 e3 = (e1,e2,e3)

type EProduct4 e1 e2 e3 e4 = (e1,e2,e3,e4)

instance
  ( EffectDom e1 s1
  , EffectDom e2 s2 )
  => EffectDom (e1,e2) (s1,s2) where
  runEffect (e1,e2) (s1,s2) =
    ( runEffect e1 s1
    , runEffect e2 s2 )

instance
  ( EffectDom e1 s1
  , EffectDom e2 s2
  , EffectDom e3 s3 )
  => EffectDom (e1,e2,e3) (s1,s2,s3) where
  runEffect (e1,e2,e3) (s1,s2,s3) =
    ( runEffect e1 s1
    , runEffect e2 s2
    , runEffect e3 s3 )

instance
  ( EffectDom e1 s1
  , EffectDom e2 s2
  , EffectDom e3 s3
  , EffectDom e4 s4 )
  => EffectDom (e1,e2,e3,e4) (s1,s2,s3,s4) where
  runEffect (e1,e2,e3,e4) (s1,s2,s3,s4) =
    ( runEffect e1 s1
    , runEffect e2 s2
    , runEffect e3 s3
    , runEffect e4 s4 )

{-| 'ECounter' provides addition, subtraction, and (positive)
  multiplication effects on 'Num' values.
-}
data ECounter n
  = ECounter { _mulAmt :: n
             , _addAmt :: n }
  deriving (Show,Eq,Ord,Generic)

makeLenses ''ECounter

instance (Num n) => Semigroup (ECounter n) where
  ECounter m2 a2 <> ECounter m1 a1 =
    -- Distributing multiplication over addition.
    ECounter (m1 + m2) (a1 * m2 + a2)

mulId :: (Num n) => n
mulId = 1

addId :: (Num n) => n
addId = 0

instance (Num n) => Monoid (ECounter n) where
  mempty = ECounter mulId addId

instance (Num n) => EffectDom (ECounter n) n where
  runEffect (ECounter m a) s = s * m + a

{-| Add @n@ to the state, where @n >= 0@.  A negative @n@ will produce a
  generate a runtime error.

@
'runEffect' ('addE' 1) 2 = 3

'runEffect' ('addE' 0) = 'Data.Function.id'
@
-}
addE :: (Ord n, Num n) => n -> ECounter n
addE n | n >= 0 = ECounter mulId n
       | otherwise = error $ "Negative value applied to addE."

{-| Subtract @n@ from the state, where @n >= 0@.

@
'runEffect' ('subE' 1) 3 = 2

'runEffect' ('subE' 0) = 'Data.Function.id'
@
-}
subE :: (Ord n, Num n) => n -> ECounter n
subE n | n >= 0 = ECounter mulId (-n)
       | otherwise = error $ "Negative value applied to subE."

{-| Multiply the state by @n@, where @n >= 0@.

@
'runEffect' ('mulE' 2) 3 = 6

'runEffect' ('mulE' 1) = 'Data.Function.id'
@
-}
mulE :: (Ord n, Num n) => n -> ECounter n
mulE n | n >= 0 = ECounter n addId
       | otherwise = error $ "Negative value applied to mulE."

{-| Check that an 'ECounter' effect only adds/subtracts, and does not
    multiply.
    
@
'additive' ('addE' 1) = 'True'

'additive' ('mulE' 2 'Data.Semigroup.<>' 'addE' 1) = 'False'

'additive' ('mulE' 1 'Data.Semigroup.<>' 'addE' 1) = 'True'
@
-}
additive :: (Eq n, Num n) => ECounter n -> Bool
additive (ECounter m _) = m == mulId

type EMaybe e v = EConst (EOnFunctor e) (Maybe v)

{-| Set the state to @'Data.Maybe.Just' v@. -}
insertE :: v -> EMaybe e v
insertE = EConst . Just

adjustE :: e -> EMaybe e v
adjustE = EModify . EOnFunctor

deleteE :: EMaybe e v
deleteE = EConst Nothing

{-| Effects on a 'Data.Map.Map' containing states, lifting a domain of
    effects on the individual states. -}
data EMap k e v
  = EMap { emap :: Map k (EMaybe e v) }
  deriving (Show,Eq,Ord,Generic)

-- EMap can't simply be a type alias for Map, since Map already has a
-- Monoid instance which overwrites same-key entries rather than
-- monoidally combining them like we want to.

instance (Ord k, EffectDom e v) => Semigroup (EMap k e v) where
  EMap a2 <> EMap a1 = EMap $ Map.unionWith (<>) a2 a1

instance (Ord k, EffectDom e v) => Monoid (EMap k e v) where
  mempty = EMap Map.empty

instance (Ord k, EffectDom e v) => EffectDom (EMap k e v) (Map k v) where
  runEffect (EMap es) s =
    let -- f :: k -> EConst (EOnFunctor e) (Maybe v) -> Map k v -> Map k v
        f k (EConst (Just v)) s = Map.insert k v s
        f k (EConst Nothing) s = Map.delete k s
        f k (EModify (EOnFunctor e)) s = Map.adjust (runEffect e) k s
    in Map.foldrWithKey' f s es

-- onKey k = EMap . Map.singleton k

-- {-| Insert an element.

-- @
-- 'runEffect' ('insertE' k v) = 'Data.Map.insert' k v
-- @
-- -}
-- insertE :: k -> v -> EMap k e v
-- insertE k v = onKey k $ EConst (Just v)

-- {-| Modify an element by running the supplied effect.  If the element is
--     not in the map, return the original map.

-- @
-- 'runEffect' ('adjustE' k e) = 'Data.Map.adjust' k ('runEffect' e)
-- @
-- -}
-- adjustE :: k -> e -> EMap k e v
-- adjustE k e = onKey k $ EModify (EOnFunctor e)

-- {-| Remove an element.

-- @
-- 'runEffect' ('deleteE' k) = 'Data.Map.delete' k
-- @
-- -}
-- deleteE :: k -> EMap k e v
-- deleteE k = onKey k $ EConst Nothing

{-| Use an 'EMaybe' effect to set the value of a key in a map.

@
'runEffect' ('mapE' k ('insertE' v)) = Data.Map.insert k v
'runEffect' ('mapE' k ('adjustE' e)) = Data.Map.adjust k ('runEffect' e)
'runEffect' ('mapE' k 'deleteE') = Data.Map.delete k
'extractEffect' k 'Data.Function..' 'injectEffect' k = 'Data.Function.id'
@
-}
mapE :: (Ord k, Eq e, Eq v, EffectDom e v)
  => k
  -> EMaybe e v
  -> EMap k e v
mapE k e | e == mempty = EMap $ Map.empty
         | otherwise = EMap $ Map.singleton k e

{-| Extract the 'EMaybe' effect on a particular key that an 'EMap'
    effect contains.  If there is no effect on that key, this will
    return 'Data.Monoid.mempty'.

@
'fromKeyE' k (mapE k ('adjustE' e) = 'adjustE' e
'fromKeyE' k1 (mapE k2 ('adjustE' e) = 'Data.Monoid.mempty'
@
-}
fromKeyE :: (Ord k, EffectDom e v)
  => k
  -> EMap k e v
  -> EMaybe e v
fromKeyE k (EMap m) = case Map.lookup k m of
                        Just e -> e
                        Nothing -> mempty
