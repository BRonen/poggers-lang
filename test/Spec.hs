module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ testCase "2+2=4" $
        (2 + 2 :: Integer) @?= (4 :: Integer),
      testCase "7 is even" $
        assertBool "Oops, 7 is odd" (odd (7 :: Integer))
    ]