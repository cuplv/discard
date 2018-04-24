{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Effect.Common where

import Data.Effect

data Counter = Add Int | Sub Int deriving (Show,Eq,Ord)

instance Monoid Counter where
  mempty = Add 0
  mappend (Add n1) (Add n2) = Add (n1 + n2)
  mappend (Add n1) (Sub n2) = Add (n1 - n2)
  mappend (Sub n1) (Add n2) = Sub (n1 - n2)
  mappend (Sub n1) (Sub n2) = Sub (n1 + n2)

instance Effect Counter where
  newtype Store Counter = Tally Int
  runEffect (Tally n1) (Add n2) = Tally (n1 + n2)
  runEffect (Tally n1) (Sub n2) = Tally (n1 - n2)

instance Monoid (Store Counter) where
  mempty = Tally 0
  mappend (Tally n1) (Tally n2) = Tally (n1 + n2)
