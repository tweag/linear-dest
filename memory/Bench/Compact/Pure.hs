{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Bench.Compact.Pure (benchmark, safety) where

import Compact.Pure.SExpr
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.ByteString.Char8 (ByteString)
import Data.Either (isRight)
import GHC.Compact (compact, getCompact)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Bench
import Test.Tasty.HUnit

benchmark :: ByteString -> Benchmark
benchmark sampleData =
  bgroup
    "compact region allocs"
    [ bgroup
        "whnfAppIO (evaluate)"
        [ bench "with dest" . (flip whnfAppIO) sampleData $ \sampleData -> do
            evaluate . parseWithDest $ sampleData,
          bench "without dest (with copy into region)" . (flip whnfAppIO) sampleData $ \sampleData -> do
            let res = parseWithoutDest sampleData
            resInRegion <- compact res
            evaluate . getCompact $ resInRegion,
          bench "without dest (with force)" . (flip whnfAppIO) sampleData $ \sampleData -> do
            let res = parseWithoutDest sampleData
            evaluate . force $ res
        ]
    ]

safety :: ByteString -> TestTree
safety sampleData =
  testGroup
    "safety"
    [ testCaseInfo "parseWithDest and parseWithoutDest give the same result and it is Right _" $ do
        let withoutDest = parseWithoutDest sampleData
            withDest = parseWithDest sampleData
        assertEqual "withoutDest == withDest" withoutDest withDest
        assertEqual "withoutDest is Right" True (isRight withoutDest)
        assertEqual "withDest is Right" True (isRight withDest)
        return ""
    ]
