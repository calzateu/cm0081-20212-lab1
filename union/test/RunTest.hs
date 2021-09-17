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

-- prop_union takes two natural automaton and a string. Then, 
-- it test if the union of that automatons accepts the string
-- and at least one of the automaton accepts the same string
prop_union :: FA Natural -> FA Natural -> String -> Bool
prop_union fa1 fa2 string
    = accepts (fa1 `union` fa2) string == (accepts fa1 string || accepts fa2 string)

-- The main function run the prop_union in quickCheck with
-- 10000 tests.
main :: IO ()
main = quickCheck $ withMaxSuccess 10000 prop_union
-------------------------------------------------------------------------------------