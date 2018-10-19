{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module CARD.Store.Bank where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State
import GHC.Generics hiding (Rep)
import Data.Aeson

import CARD.Store
import CARD.Replica
import CARD.Network
import CARD.Store.CA


deposit :: (Rep i r (CA i Counter) t) 
        => Int 
        -> RepS i r (CA i Counter) t (Either String Int)
deposit n = 
  if n > 0
     then safeEmit (ef$ Add n) >> return (Right n)
     else return (Left "Can't deposit less than 1.")

withdraw :: (Rep i r (CA i Counter) t) 
         => Int 
         -> RepS i r (CA i Counter) t (Either String Int)
withdraw n =
  if n > 0
     then do (Counter s) <- query (cr LEQ) 
             if s >= n
                then do safeEmit (ef$ Sub n)
                        release
                        return (Right n)
                else release >> return (Left "Not enough in account.")
     else return (Left "Can't withdraw less than 1.")

withdrawS :: (Rep i r (CA i Counter) t) 
          => Int 
          -> RepS i r (CA i Counter) t (Either String Int)
withdrawS = undefined

current :: (Rep i r (CA i Counter) t) => RepS i r (CA i Counter) t Int
current = do (Counter s) <- query crT
             release
             return s

currentS :: (Rep i r (CA i Counter) t) => RepS i r (CA i Counter) t Int
currentS = do (Counter s) <- query EQV
              release
              return s
