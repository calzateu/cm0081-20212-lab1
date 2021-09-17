module Union (union) where

import Numeric.Natural ( Natural )
import Data.Set ()                
import qualified Data.Set as Set
import Language.Mira.FA.Types

--------------------------------------------------------------------------
--Auxiliar Functions

-- This receives a Move or Emove and a natural number. After, 
-- it add the number to each state in the Move or Emove.
renumberAuxMoves :: Move Natural -> Natural -> Move Natural
renumberAuxMoves (Move a c b) n = Move (a+n) c (b+n) 
renumberAuxMoves (Emove a b) n = Emove (a+n) (b+n) 

-- The function takes a list of Moves and a natural. Then, it uses
-- an auxiliar function called renumberAuxMoves to add that natural 
-- to both states in the Move.
renumberMoves :: [Move Natural] -> Natural -> [Move Natural] 
renumberMoves [] _ = []
renumberMoves (x:xs) n = renumberAuxMoves x n : renumberMoves xs n 

-- This function takes a list of naturals and a natural number. Then,
-- it adds that number to each element of the list and finally adds 1
-- to each element.
renumberStates :: [Natural] -> Natural -> [Natural]
renumberStates [] _ = []
renumberStates (x:xs) n = x+n+1 : renumberStates xs n 

--------------------------------------------------------------------------
--Function Union

-- This function takes two automaton and returns the union of them.
-- The function renumber the states and the moves thanks to other
-- auxiliar functions. The idea is renumber the states to avoid
-- possible repetitions and replace the new states on the moves.
-- To renumber the states we find the max number state in the first 
-- automaton and we add that to the states of the second automaton. 
-- Then we add 1 in the states of both automaton to avoid possible
-- errors with zero.
union :: FA Natural -> FA Natural -> FA Natural
union (MkFA fa1States fa1Moves fa1StartState fa1FinalStates)
      (MkFA fa2States fa2Moves fa2StartState fa2FinalStates)

    = MkFA (Set.union (Set.union states1 states2) newstates)
           (Set.union (Set.union moves1 moves2) newmoves)
           0
           (Set.union (Set.fromList (renumberStates (Set.toList fa1FinalStates) 0)) (Set.fromList (renumberStates (Set.toList fa2FinalStates) nmax)))

      where
      states1 = Set.fromList (renumberStates (Set.toList fa1States) 0)
      nmax = Set.findMax states1
      states2 = Set.fromList (renumberStates (Set.toList fa2States) nmax)
      newstates = Set.singleton 0
      newmoves = Set.fromList [Emove 0 (fa1StartState+1), Emove 0 (fa2StartState+nmax+1)]
      moves1 = Set.fromList (renumberMoves (Set.toList fa1Moves) 1)
      moves2 = Set.fromList (renumberMoves (Set.toList fa2Moves) (nmax+1))
--------------------------------------------------------------------------
