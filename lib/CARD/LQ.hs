{-# LANGUAGE GADTs #-}

module CARD.LQ where

import CARD.Store

data LQ s a where
  Const :: c -> LQ s c
  Commit :: (Store s) => Effect s -> LQ s a -> LQ s a
  Query :: (Store s) => Conref s -> (s -> LQ s a) -> LQ s a

instance Functor (LQ s) where
  fmap f m = case m of
    Const c -> Const (f c)
    Commit e t -> Commit e (fmap f t)
    Query c f2 -> Query c (\s -> fmap f (f2 s))

instance Applicative (LQ s) where
  pure = Const
  (<*>) tf t = case tf of
    Const f -> fmap f t
    Commit e f -> Commit e (f <*> t)
    Query c f2 -> Query c (\s -> (f2 s) <*> t)

instance Monad (LQ s) where
  (>>=) t1 t2 = case t1 of
    Const c -> t2 c
    Commit e t3 -> Commit e (t3 >>= t2)
    Query c f -> Query c (\s -> f s >>= t2)

commit :: (Store s) => Effect s -> LQ s ()
commit e = Commit e (Const ())

query :: (Store s) => Conref s -> LQ s s
query c = Query c (\s -> Const s)

dp :: Int -> LQ Counter (Either String Int)
dp n = do commit (ef$ Add n)
          return (Right n)

wd :: Int -> LQ Counter (Either String Int)
wd n = do (Counter s) <- query (cr$ LEQ)
          if s >= n
             then do commit (ef$ Sub n)
                     return (Right n)
             else return (Left "Not enough in account.")
