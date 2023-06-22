module Main (main) where

import qualified Bench.Compact.Pure as Compact
import Bench.Compact.SExpr
import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import System.Environment
import Test.Tasty.Bench (defaultMain)

-- Launch regular benchmark with
-- stack bench linear-dest:bench:memory --ghc-options '-threaded -O2 -rtsopts' --ba '+RTS -T -N -RTS'
-- cabal bench -w /home/thomas/tweag/ghc/_build/stage1/bin/ghc --allow-newer --enable-library-profiling --enable-executable-profiling --ghc-options='-threaded -O2 -rtsopts' linear-dest:bench:memory --benchmark-options='+RTS -T -N -RTS'

-- Profile parseWithoutDest with
-- stack bench --library-profiling --executable-profiling --ghc-options '-threaded -O2 -rtsopts -fprof-late' linear-dest:bench:memory --ba '+RTS -p -N -RTS runParseWithoutDest'
-- cabal bench -w /home/thomas/tweag/ghc/_build/stage1/bin/ghc --allow-newer --enable-library-profiling --enable-executable-profiling --ghc-options='-prof -threaded -O2 -rtsopts -fprof-late' linear-dest:bench:memory --benchmark-options='+RTS -p -N -RTS runParseWithoutDest'

-- Profile parseUsingDest with
-- stack bench --library-profiling --executable-profiling --ghc-options '-threaded -O2 -rtsopts -fprof-late' linear-dest:bench:memory --ba '+RTS -p -N -RTS runParseUsingDest'
-- cabal bench -w /home/thomas/tweag/ghc/_build/stage1/bin/ghc --allow-newer --enable-library-profiling --enable-executable-profiling --ghc-options='-prof -threaded -O2 -rtsopts -fprof-late' linear-dest:bench:memory --benchmark-options='+RTS -p -N -RTS runParseUsingDest'

-- remove useless lines in profiling results with
-- .*?0\.0    0\.0     0\.0    0\.0\n

-- remove all lines with no individual contribution to alloc with
-- .*?([0-9]+\.[0-9])\s+?0\.0\s+?([0-9]+\.[0-9])\s+?([0-9]+\.[0-9])\n

-- remove all lines with no inherited contribution to alloc with
-- .*?([0-9]+\.[0-9])\s+?([0-9]+\.[0-9])\s+?([0-9]+\.[0-9])\s+?0\.0\n

main :: IO ()
main = do
  args <- getArgs
  case args of
    "runParseWithoutDest" : _ -> do
      sampleData <- loadSampleData
      let res = parseWithoutDest sampleData
      evaluate $ rnf $ res
    "runParseUsingDest" : _ -> do
      sampleData <- loadSampleData
      let res = parseUsingDest sampleData
      evaluate $ rnf $ res
    _ ->
      defaultMain
        [ Compact.benchmarks
        ]
