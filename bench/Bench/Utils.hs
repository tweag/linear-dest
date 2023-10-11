{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-x-partial #-}

module Bench.Utils where

import Control.DeepSeq
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseInfo, assertEqual)
import Test.Tasty.Bench
import Control.Exception (evaluate)
import Data.Functor ((<&>))
import GHC.Compact (compact, getCompact)

safetySameAsFirstImpl :: forall m a r. (Show r, Eq r) => [(a %m -> r, String, Bool)] -> [(IO a, String)] -> IO TestTree
safetySameAsFirstImpl impls datasets = do
  let ((refImpl, refImplName, _):otherImpls) = impls
  testGroup "safety" <$> (sequence $
    datasets <&> \(loadSampleData, sizeName) -> do
      sampleData <- loadSampleData
      return $ testGroup sizeName $ otherImpls <&> \(impl, implName, _) ->
          testCaseInfo (refImplName ++ " and " ++ implName ++ " give the same result") $ do
            let expected = refImpl sampleData
                actual = impl sampleData
            assertEqual "same result" expected actual
            return "")

benchImpls :: forall m a r. (NFData r) => [(a %m -> r, String, Bool)] -> [(IO a, String)] -> IO Benchmark
benchImpls impls datasets = do
  bgroup "benchmark" <$> (sequence $
    datasets <&> \(loadSampleData, sizeName) -> do
      sampleData <- loadSampleData
      return $ testGroup sizeName $ concat $ impls <&> \(impl, implName, isLazy) -> if isLazy
          then
            [ bench (implName ++ ".force") $ (flip whnfAppIO) sampleData $ \sampleData -> evaluate $ force $ impl sampleData,
              bench (implName ++ ".copyIntoReg") $ (flip whnfAppIO) sampleData $ \sampleData -> do
                    resInRegion <- compact $ impl sampleData
                    evaluate $ getCompact $ resInRegion
            ]
          else
            [ bench implName $ (flip whnfAppIO) sampleData $ \sampleData -> evaluate $ impl sampleData ])

launchImpl :: forall m a r. (NFData r) => String -> [(a %m -> r, String, Bool)] -> [(IO a, String)] -> IO ()
launchImpl requestedImplDataSetspec impls datasets = go impls (go' datasets) where
  (requestedSize, dotRequestedImplSpec) = span (/= '.') requestedImplDataSetspec
  (requestedImplRadical, requestedImplVariant) = span (/= '.') (tail dotRequestedImplSpec)
  go [] _ = error ("requested implementation '" ++ requestedImplRadical ++ "' not found")
  go ((impl, implName, isLazy):_) loadSampleData | implName == requestedImplRadical = do
    sampleData <- loadSampleData
    if isLazy
      then case requestedImplVariant of
        ".force" -> evaluate $ rwhnf $ force $ impl sampleData
        ".copyIntoReg" -> do
          resInRegion <- compact $ impl sampleData
          evaluate $ rwhnf $ getCompact $ resInRegion
        _ -> error ("variant '" ++ requestedImplVariant ++ "' not found (required for lazy impl)")
    else
      evaluate $ rwhnf $ impl sampleData
    putStrLn "Done!"
  go (_:xs) loadSampleData = go xs loadSampleData

  go' [] = error ("requested size '" ++ requestedSize ++ "' not found")
  go' ((loadSampleData, sizeName):_) | sizeName == requestedSize = loadSampleData
  go' (_:xs) = go' xs
