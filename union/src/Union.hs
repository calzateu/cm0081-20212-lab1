module Union (union) where

import Numeric.Natural ( Natural )
import Data.Set ( Set )                 
import qualified Data.Set as Set
import Language.Mira.FA.Types

--------------------------------------------------------------------------
--Auxiliar Functions

renumberAux :: Move Natural -> Natural -> Move Natural
renumberAux (Move a c b) n = Move (a+n) c (b+n) 
renumberAux (Emove a b) n = Emove (a+n) (b+n) 

renumberSet :: [Move Natural] -> Natural -> [Move Natural] 
renumberSet [] n = []
renumberSet (x:xs) n = ([renumberAux x n]) ++ (renumberSet xs n) 

renumberFinalStates :: [Natural] -> Natural -> [Natural]
renumberFinalStates [] n = []
renumberFinalStates (x:xs) n = ([x + n]) ++ (renumberFinalStates xs n)

--------------------------------------------------------------------------
--Function Union

union :: FA Natural -> FA Natural -> FA Natural
union (MkFA fa1States fa1Moves fa1StartState fa1FinalStates)
      (MkFA fa2States fa2Moves fa2StartState fa2FinalStates)

    = MkFA (Set.union (Set.union states1 states2) newstates)
           (Set.union (Set.union moves1 moves2) newmoves)
           (0)
           (Set.union fa1FinalStates (Set.fromList (renumberFinalStates (Set.toList fa2FinalStates) n)))

      where
      n = fromIntegral (Set.size fa1States)
      m = fromIntegral (Set.size fa2States)
      states1 = Set.fromList [0..n]
      states2 = Set.fromList [n+1..n+m]
      newstates = Set.fromList [0,(n+m+1)]
      newmoves = Set.fromList [Emove 0 1, Emove 0 (n+1)] 
      moves1 = fa1Moves
      moves2 = Set.fromList (renumberSet (Set.toList fa2Moves) n) 

--------------------------------------------------------------------------
