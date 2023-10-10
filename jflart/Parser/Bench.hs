{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-x-partial #-}

module Parser.Bench (benchmark, safety, getBenchgroup) where

import Compact.Pure.SExpr
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.ByteString.Char8 (ByteString)
import Data.Either (isRight)
import Data.Functor ((<&>))
import GHC.Compact (compact, getCompact)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Bench
import Test.Tasty.HUnit

impls :: [(ByteString -> Either SExprParseError SExpr, String, Bool)]
impls =
  [ (parseWithoutDest, "parseWithoutDest", True)
  , (parseWithDest, "parseWithDest", False)
  ]

benchmark :: ByteString -> Benchmark
benchmark sampleData =
  bgroup "benchmark" $
    concat $ impls <&> \(impl, implName, isLazy) -> if isLazy
      then
        [ bench (implName ++ " (with force)") $ (flip whnfAppIO) sampleData $ \sampleData -> evaluate $ force $ impl sampleData,
          bench (implName ++ " (with copy into region)") $ (flip whnfAppIO) sampleData $ \sampleData -> do
                resInRegion <- compact $ impl sampleData
                evaluate $ getCompact $ resInRegion
        ]
      else
        [ bench implName $ (flip whnfAppIO) sampleData $ \sampleData -> evaluate $ impl sampleData ]

safety :: ByteString -> TestTree
safety sampleData =
  testGroup "safety" $
    (tail impls) <&> \(impl, implName, _) ->
      testCaseInfo ("parseWithoutDest and " ++ implName ++ " give the same result") $ do
        let expected = parseWithoutDest sampleData
            actual = impl sampleData
        assertEqual "same result" expected actual
        assertEqual (implName ++ " is Right") True (isRight actual)
        return $ "<too large to display>"

getBenchgroup :: IO Benchmark
getBenchgroup = do
  !sampleData <- loadSampleData
  return $ bgroup "s-expression-parser"
    [ benchmark sampleData,
      safety sampleData
    ]
