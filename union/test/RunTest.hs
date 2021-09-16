{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Language.Mira.FA.Types
import Language.Mira.FA.Implement(accepts)
import Numeric.Natural (Natural)
import Test.QuickCheck

import Union

instance Arbitrary Natural where
      arbitrary = arbitrarySizedNatural
      shrink = shrinkIntegral

prop_union :: FA Natural -> FA Natural -> String -> Bool
prop_union fa1 fa2 string
    = accepts (fa1 `union` fa2) string == (accepts fa1 string || accepts fa2 string)

main :: IO ()
main = quickCheck $ withMaxSuccess 10000 prop_union
