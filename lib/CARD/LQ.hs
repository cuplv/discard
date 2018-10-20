module CARD.LQ where

import Control.Monad.Free

import CARD.Store


data LQ s a = Issue (Effect s) a
            | Query (Conref s) (s -> a)

instance Functor (LQ s) where
  fmap f m = case m of
    Issue e a -> Issue e (f a)
    Query c a -> Query c (\s -> f (a s))

type Op s = Free (LQ s)

issue :: (Store s) => Effect s -> Op s ()
issue e = Free (Issue e (Pure ()))

query :: (Store s) => Conref s -> Op s s
query c = Free (Query c Pure)

assert :: Bool -> String -> Op s (Either String a) -> Op s (Either String a)
assert b s op = if b
                   then op
                   else return (Left s)
