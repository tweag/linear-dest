{-# LANGUAGe BlockArguments #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Bench.Compact.Pure (benchmark, safety) where

import GHC.Compact (compact, getCompact)
import Test.Tasty.Bench
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Compact.Pure.SExpr
import Test.Tasty.HUnit
import Test.Tasty (testGroup, TestTree)
import Data.ByteString.Char8 (ByteString)
import Data.Either (isRight)

benchmark :: ByteString -> Benchmark
benchmark sampleData =
    bgroup
      "compact region allocs"
      [
        bgroup
          "whnfAppIO (evaluate)"
          [ bench "without dest (with force)" . (flip whnfAppIO) sampleData $ \sampleData -> do
              let res = parseWithoutDest sampleData
              evaluate . force $ res
            ,
            bench "without dest (with copy into region)" . (flip whnfAppIO) sampleData $ \sampleData -> do
              let res = parseWithoutDest sampleData
              resInRegion <- compact res
              evaluate . getCompact $ resInRegion
            ,
            bench "with dest" . (flip whnfAppIO) sampleData $ \sampleData -> do
              evaluate . parseWithDest $ sampleData
          ]
      ]

safety :: ByteString -> TestTree
safety sampleData =
  testGroup "safety" [testCaseInfo "parseWithDest and parseWithoutDest give the same result and it is Right _" $ do
        let withoutDest = parseWithoutDest sampleData
            withDest = parseWithDest sampleData
        assertEqual "withoutDest == withDest" withoutDest withDest
        assertEqual "withoutDest is Right" True (isRight withoutDest)
        assertEqual "withDest is Right" True (isRight withDest)
        return ""
    ]
