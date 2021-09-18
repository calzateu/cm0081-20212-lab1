{-# OPTIONS_GHC -Wno-orphans #-}

------------------------------------------------------------------------------------
module Main (main) where

import Language.Mira.FA.Types
import Language.Mira.FA.Implement(accepts)
import Numeric.Natural (Natural)
import Test.QuickCheck
import Union
-------------------------------------------------------------------------------------
-- Arbitrary instances for @Natural@ from quickeck-instances 0.3.25.2.

instance Arbitrary Natural where
    arbitrary = arbitrarySizedNatural
    shrink    = shrinkIntegral

-------------------------------------------------------------------------------------
--Prop functions

-- prop_union takes two natural automaton and a list of strings.  
-- Then, it test if the union of that automatons accepts the head
-- of the list and at least one of the automaton accepts the same 
-- head. Then we make the recursive call.
prop_union :: FA Natural -> FA Natural -> [String] -> Bool
prop_union _ _ [] = True
prop_union fa1 fa2 (x:xs)
    = (accepts (fa1 `union` fa2) x == (accepts fa1 x || accepts fa2 x)) &&
      (prop_union fa1 fa2 xs)
      
-- The main function run the prop_union in quickCheck with
-- 10000 tests.
main :: IO ()
main = quickCheck $ withMaxSuccess 10000 prop_union
-------------------------------------------------------------------------------------