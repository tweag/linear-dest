{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-x-partial #-}

module Bench.Parser (impls, extraSafety, dataSets) where

import Compact.Pure.SExpr
import Data.ByteString.Char8 (ByteString)
import Data.Either (isRight)
import Data.Functor ((<&>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

impls :: [(ByteString -> Either SExprParseError SExpr, String, Bool)]
impls =
  [ (parseWithoutDest, "parseWithoutDest", True)
  , (parseWithDest, "parseWithDest", False)
  ]

extraSafety :: IO TestTree
extraSafety =
  testGroup "extraSafety" <$> (sequence $
    dataSets <&> \(loadSampleData, sizeName) -> do
      sampleData <- loadSampleData
      return $ testGroup sizeName $ impls <&> \(impl, implName, _) ->
          testCaseInfo (implName ++ " is Right") $ do
            assertEqual (implName ++ " is Right") True (isRight (impl sampleData))
            return $ "")
