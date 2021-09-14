module Union (union) where

import Numeric.Natural ( Natural )
import Data.Set ( Set )                 
import qualified Data.Set as Set
import Language.Mira.FA.Types

union :: FA Int -> FA Int -> FA Int
union ((MkFA fa1States fa1Moves fa1StartState fa1FinalStates))
      ((MkFA fa2States fa2Moves fa2StartState fa2FinalStates)) 

    = (MkFA (Set.union (Set.union states1 states2) newstates)
            (Set.union (Set.union moves1 moves2) newmoves)
            (0)
            (Set.fromList [n+m+1]))

    where
    n = Set.size fa1States
    m = Set.size fa2States
    states1 = Set.fromList [0..n]
    states2 = Set.fromList [n+1..n+m]
    newstates = Set.fromList [0,(n+m+1)]
    newmoves = Set.fromList [Emove 0 1, Emove 0 (n+1), Emove n (n+m+1), Emove (n+m) (n+m+1)] 
    moves1 = fa1Moves
    moves2 = Set.fromList [Emove 0 0]
