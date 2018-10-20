{-# LANGUAGE GADTs #-}

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

assert :: Bool -> String -> Op s (Either String ())
assert b s = return (if b
                        then Right ()
                        else Left s)

dp :: Int -> Op Counter (Either String Int)
dp n = do assert (n > 0) "Must deposit at least 1."
          issue (ef$ Add n)
          return (Right n)

wd :: Int -> Op Counter (Either String Int)
wd n = do assert (n > 0) "Must withdraw at least 1."
          (Counter s) <- query (cr$ LEQ)
          if s >= n
             then do issue (ef$ Sub n)
                     return (Right n)
             else return (Left "Not enough in account.")
