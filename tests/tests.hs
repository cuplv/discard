module Main where

import System.Exit
import Control.Monad (foldM)

import CARD
import CARD.EventGraph.SetEG
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (sort)

mks :: (Ord a) => [a] -> Set a
mks = Set.fromList

main :: IO ()
main = do
  es1 === tes e11
  es2 === tes e21
  es2 === tes e22
  emit e11 (tes e21) === tes e3
  emit e11 (tes e12) === tes e13
  let a4321 = 4 <: 3 <: 2 <# 1
      es234 = [ 2 <# 1, 3 <# 1, eff 4 ]
      es434 = [ 4<:3<:2<#1, 3<#1, eff 4 ]
  emit a4321 (tes es234) === tes es434
  -- l <- serialize e3
  -- l === [1,2,98,99,3,3]
  ee3 <- edge e3
  Set.fromList ee3 === Set.fromList [(3,tes [2 <# 1])
                                    ,(3,tes [eff 98,eff 99])]
  merge (tes [2 <# 1]) (tes [eff 98,eff 99]) =*= pure (tes [2<#1,eff 98,eff 99])
  (mks <$> edge (tes [2<#1,eff 98, eff 99])) =*= pure (mks [(2,tes $ eff 1)
                                                           ,(98,empty)
                                                           ,(99,empty)])
  foldM merge empty [tes $ eff 1,empty,empty] =*= pure (tes $ eff 1)
  serialize (tes [3<#1, 2<#1]) =*= pure (1 : (reverse.sort) [2,3])
  serialize (tes (eff 1)) =*= pure [1]
  return ()


(===) :: (Show a, Eq a) => a -> a -> IO ()
(===) a b = if a == b
               then return ()
               else die $ show a ++ "\n  did not equal\n" ++ show b ++ "."

(=*=) :: (Show a, Eq a) => IO a -> IO a -> IO ()
(=*=) a b = do ioa <- a
               iob <- b
               ioa === iob

es1 = SetEG (Set.fromList 
  [SetEff 3 (SetEG (Set.fromList 
    [SetEff 2 (SetEG (Set.fromList 
      [SetEff 1 emptySetEG]))]))])

e11 = 3 <: 2 <: eff 1

e111 = 4 <: e11

e12 = [2 <: eff 1, 3 <: eff 1]

e13 = [3 <: 2 <: eff 1, 3 <: eff 1]

es2 = SetEG (Set.fromList
  [SetEff 3 (SetEG (Set.fromList
    [SetEff 98 emptySetEG
    ,SetEff 99 emptySetEG]))])

e21 = 3 <: [eff 98, eff 99]

e22 = 3 <: [eff 98, eff 99, eff 99]
     
e3 = tes [3 <: [2 <: eff 1], 3 <: [eff 98, eff 99]]
