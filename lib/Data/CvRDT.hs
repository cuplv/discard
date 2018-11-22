{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.CvRDT
  ( CvRDT (..)
  ) where

class CvRDT r s m where
  merge :: r -> s -> s -> m s
  cvempty :: r -> m s

instance (Applicative m, CvRDT r1 s1 m, CvRDT r2 s2 m) => CvRDT (r1,r2) (s1,s2) m where
  merge (r1,r2) (s1,s2) (s3,s4) = (,) <$> merge r1 s1 s3 <*> merge r2 s2 s4
  cvempty (r1,r2) = (,) <$> cvempty r1 <*> cvempty r2
