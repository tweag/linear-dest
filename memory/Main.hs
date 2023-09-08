{-# LANGUAGE BangPatterns #-}

module Main (main) where

import qualified Bench.Compact.Pure as Compact
import Compact.Pure.SExpr
import Control.DeepSeq (rwhnf, force)
import Control.Exception (evaluate)
import System.Environment
import Test.Tasty.Bench (defaultMain)
import GHC.Compact (compact, getCompact)

-- Launch regular benchmark with
-- cabal bench -w /home/thomas/tweag/ghc/_build/stage1/bin/ghc --allow-newer --ghc-options='-threaded -O2 -rtsopts' linear-dest:bench:memory --benchmark-options='+RTS -T -N1 -RTS'

-- Profile parseWithoutDestForce with
-- cabal bench -w /home/thomas/tweag/ghc/_build/stage1/bin/ghc --allow-newer --enable-profiling --profiling-detail=late --ghc-options='-threaded -O2 -rtsopts' linear-dest:bench:memory --benchmark-options='+RTS -p -N1 -RTS runParseWithoutDestForce' && mv memory.prof memory_without_dest_force.prof

-- Profile parseWithoutDestCopyReg with
-- cabal bench -w /home/thomas/tweag/ghc/_build/stage1/bin/ghc --allow-newer --enable-profiling --profiling-detail=late --ghc-options='-threaded -O2 -rtsopts' linear-dest:bench:memory --benchmark-options='+RTS -p -N1 -RTS runParseWithoutDestCopyReg' && mv memory.prof memory_without_dest_copy_reg.prof

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
    "runParseWithoutDestForce" : _ -> do
      let res = parseWithoutDest sampleData
      evaluate . rwhnf . force $ res
    "runParseWithoutDestCopyReg" : _ -> do
      let res = parseWithoutDest sampleData
      compResInRegion <- compact res
      evaluate . rwhnf . getCompact $ compResInRegion
    "runParseWithDest" : _ -> do
      let resInRegion = parseWithDest sampleData
      evaluate . rwhnf $ resInRegion
    _ ->
      defaultMain
        [ Compact.benchmark sampleData
        , Compact.safety sampleData
        ]
