{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Effect where

import Data.EventGraph

class (Monoid e, Monoid (Store e)) => Effect e where
  data Store e
  runEffect :: Store e -> e -> Store e

evalHistory :: (EGMonad g e m, Effect e) => g e -> m (Store e)
evalHistory = foldg runEffect mempty
