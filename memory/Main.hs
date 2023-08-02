{-# LANGUAGE BangPatterns #-}

module Main (main) where

import qualified Bench.Compact.Pure as Compact
import Bench.Compact.SExpr
import Control.DeepSeq (rnf, force)
import Control.Exception (evaluate)
import System.Environment
import Test.Tasty.Bench (defaultMain)
import Test.Tasty.HUnit
import Test.Tasty (testGroup)
import GHC.Compact (compact, getCompact)
import Data.Either (isRight)

-- Launch regular benchmark with
-- stack bench linear-dest:bench:memory --ghc-options '-threaded -O2 -rtsopts' --ba '+RTS -T -N1 -RTS'
-- cabal bench -w /home/tbagrel/tweag/ghc/_build/stage1/bin/ghc --allow-newer --enable-library-profiling --enable-executable-profiling --ghc-options='-threaded -O2 -rtsopts' linear-dest:bench:memory --benchmark-options='+RTS -T -N1 -RTS'

-- Profile parseWithoutDest with
-- stack bench --library-profiling --executable-profiling --ghc-options '-threaded -O2 -rtsopts -fprof-late' linear-dest:bench:memory --ba '+RTS -p -N1 -RTS runParseWithoutDest' && mv memory.prof memory_without_dest.prof
-- cabal bench -w /home/tbagrel/tweag/ghc/_build/stage1/bin/ghc --allow-newer --enable-library-profiling --enable-executable-profiling --ghc-options='-prof -threaded -O2 -rtsopts -fprof-late' linear-dest:bench:memory --benchmark-options='+RTS -p -N1 -RTS runParseWithoutDest' && mv memory.prof memory_without_dest.prof

-- Profile parseUsingDest with
-- stack bench --library-profiling --executable-profiling --ghc-options '-threaded -O2 -rtsopts -fprof-late' linear-dest:bench:memory --ba '+RTS -p -N1 -RTS runParseUsingDest' && mv memory.prof memory_using_dest.prof
-- cabal bench -w /home/tbagrel/tweag/ghc/_build/stage1/bin/ghc --allow-newer --enable-library-profiling --enable-executable-profiling --ghc-options='-prof -threaded -O2 -rtsopts -fprof-late' linear-dest:bench:memory --benchmark-options='+RTS -p -N1 -RTS runParseUsingDest' && mv memory.prof memory_using_dest.prof

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
    "runParseUsingDest" : _ -> do
      let resInRegion = parseUsingDest sampleData
      evaluate . rnf $ resInRegion
    _ ->
      defaultMain
        [ Compact.benchmarks
        , testGroup "safety"
            [testCaseInfo "parseUsingDest and parseWithoutDest give the same result and it is Right _" (sameResult sampleData)]
        ]

sameResult :: String -> IO String
sameResult sampleData = do
  let withoutDest = parseWithoutDest sampleData
      usingDest = parseUsingDest sampleData
  assertEqual "withoutDest == usingDest" withoutDest usingDest
  assertEqual "withoutDest is Right" True (isRight withoutDest)
  assertEqual "usingDest is Right" True (isRight usingDest)
  return ""
