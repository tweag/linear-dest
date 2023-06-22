{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.Compact.Pure (compactPureTests)
import Test.Tasty

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests =
  testGroup
    "All tests"
    [ testGroup
        "Functional tests"
        [ compactPureTests
        ]
    ]
