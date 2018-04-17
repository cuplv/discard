module Main where

import System.Exit

import Data.EventGraph.SetEG
import Data.Set (Set)
import qualified Data.Set as Set

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
  return ()


(===) :: (Show a, Eq a) => a -> a -> IO ()
(===) a b = if a == b
               then return ()
               else die $ show a ++ "\n  did not equal\n" ++ show b ++ "."

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


