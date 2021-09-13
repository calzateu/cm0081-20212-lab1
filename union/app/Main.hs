module Main where

-- Para crear los autóamatas mecesito estos datos
import Data.Set ( Set )
import qualified Data.Set as Set

import Composition

import Language.Mira.FA.Types

fa1 :: FA Int
fa1 = MkFA 
  (Set.fromList [1,2,3])
  (Set.fromList [Move 1 'a' 2, Emove 3 3])
  2
  (Set.fromList [2])

fa2 :: FA Int
fa2 = MkFA 
  (Set.fromList [1,2,3])
  (Set.fromList [Move 1 'a' 2, Emove 3 3])
  2
  (Set.fromList [2])

main :: IO ()
-- main = someFunc
main = print $ union fa1 fa2

