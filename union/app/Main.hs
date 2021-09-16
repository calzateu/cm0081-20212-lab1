module Main where

import Union

import Numeric.Natural (Natural)
import Data.Set ( Set )                 
import qualified Data.Set as Set              

import Language.Mira.FA.Types           

----------------------------------------------
-- Automaton accepting strings ending in 01
fa1 :: FA Natural
fa1 = MkFA (Set.fromList [55,24,77])
           (Set.fromList [Move 55 '1' 55, Move 55 '0' 24, Move 24 '1' 77, Move 24 '0' 24, Move 77 '1' 55, Move 77 '0' 24])
           55
           (Set.fromList [77]) 

-- Automaton accepting strings begining in 10
fa2 :: FA Natural
fa2 = MkFA (Set.fromList [45,53,0])
           (Set.fromList[Move 45 '1' 53, Move 53 '0' 0, Move 0 '1' 0, Move 0 '0' 0])
           45
           (Set.fromList [0]) 
----------------------------------------------

main :: IO ()
main = print $ union fa1 fa2