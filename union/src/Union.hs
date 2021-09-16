module Union (union) where

import Numeric.Natural ( Natural )
import Data.Set ()                
import qualified Data.Set as Set
import Language.Mira.FA.Types

--------------------------------------------------------------------------
--Auxiliar Functions

renumberAuxMoves :: Move Natural -> Natural -> Move Natural
renumberAuxMoves (Move a c b) n = Move (a+n) c (b+n) 
renumberAuxMoves (Emove a b) n = Emove (a+n) (b+n) 

renumberMoves :: [Move Natural] -> Natural -> [Move Natural] 
renumberMoves [] _ = []
renumberMoves (x:xs) n = renumberAuxMoves x n : renumberMoves xs n 

renumberStates :: [Natural] -> Natural -> [Natural]
renumberStates [] _ = []
renumberStates (x:xs) n = x+n+1 : renumberStates xs n 

--------------------------------------------------------------------------
--Function Union

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
