{-# LANGUAGE BangPatterns #-}

module Main (main) where

import qualified Bench.Compact.Pure as Compact
import Compact.Pure.SExpr
import Control.DeepSeq (rnf, force)
import Control.Exception (evaluate)
import System.Environment
import Test.Tasty.Bench (defaultMain)
import Test.Tasty.HUnit
import Test.Tasty (testGroup)
import GHC.Compact (compact, getCompact)
import Data.ByteString.Char8 (ByteString)
import Data.Either (isRight)

-- Launch regular benchmark with
-- cabal bench -w /home/thomas/tweag/ghc/_build/stage1/bin/ghc --allow-newer --enable-profiling --profiling-detail=late --ghc-options='-threaded -O2 -rtsopts' linear-dest:bench:memory --benchmark-options='+RTS -T -N1 -RTS'

-- Profile parseWithoutDest with
-- cabal bench -w /home/thomas/tweag/ghc/_build/stage1/bin/ghc --allow-newer --enable-profiling --profiling-detail=late --ghc-options='-threaded -O2 -rtsopts' linear-dest:bench:memory --benchmark-options='+RTS -p -N1 -RTS runParseWithoutDest' && mv memory.prof memory_without_dest.prof

-- Profile parseWithDest with
-- cabal bench -w /home/thomas/tweag/ghc/_build/stage1/bin/ghc --allow-newer --enable-profiling --profiling-detail=late --ghc-options='-threaded -O2 -rtsopts' linear-dest:bench:memory --benchmark-options='+RTS -p -N1 -RTS runParseWithDest' && mv memory.prof memory_with_dest.prof

-- remove useless lines in profiling results with
-- .*?0\.0    0\.0     0\.0    0\.0\n

-- remove all lines with no individual contribution to alloc with
-- .*?([0-9]+\.[0-9])\s+?0\.0\s+?([0-9]+\.[0-9])\s+?([0-9]+\.[0-9])\n

-- remove all lines with no inherited contribution to alloc with
-- .*?([0-9]+\.[0-9])\s+?([0-9]+\.[0-9])\s+?([0-9]+\.[0-9])\s+?0\.0\n

main :: IO ()
main = do
  args <- getArgs
  !sampleData <- evaluate =<< force <$> loadSampleData
  case args of
    "runParseWithoutDest" : _ -> do
      let res = parseWithoutDest sampleData
      compResInRegion <- compact res
      evaluate . rnf . getCompact $ compResInRegion
    "runParseWithDest" : _ -> do
      let resInRegion = parseWithDest sampleData
      evaluate . rnf $ resInRegion
    _ ->
      defaultMain
        [ Compact.benchmarks
        , testGroup "safety"
            [testCaseInfo "parseWithDest and parseWithoutDest give the same result and it is Right _" (sameResult sampleData)]
            -- testCaseInfo "display (TODO remove)" (displayResult sampleData),
        ]

-- displayResult :: ByteString -> IO String
-- displayResult sampleData = do
--   putStrLn $ "withoutDest = " ++ (show $ parseWithoutDest sampleData)
--   putStrLn $ "withDest = " ++ (show $ parseWithDest sampleData)
--   return ""

sameResult :: ByteString -> IO String
sameResult sampleData = do
  let withoutDest = parseWithoutDest sampleData
      withDest = parseWithDest sampleData
  assertEqual "withoutDest == withDest" withoutDest withDest
  assertEqual "withoutDest is Right" True (isRight withoutDest)
  assertEqual "withDest is Right" True (isRight withDest)
  return ""
