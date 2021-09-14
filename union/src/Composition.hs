module Union (union) where

import Numeric.Natural ( Natural )
import Data.Set ( Set )                 
import qualified Data.Set as Set
import Language.Mira.FA.Types

union :: FA Natural -> FA Natural -> Int
union fa1 fa2 = 42