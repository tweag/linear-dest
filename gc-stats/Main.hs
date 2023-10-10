{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}

module Main (main) where

import Compact.Pure.SExpr
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import GHC.Compact (compact, getCompact)
import System.Environment

-- Get detailed GC stats with
-- cabal run -w $(pwd)/ghc@580d39a221/bin/ghc --allow-newer linear-dest:exe:gc-stats -- +RTS -s -RTS runParseWithoutDestForce
-- cabal run -w $(pwd)/ghc@580d39a221/bin/ghc --allow-newer linear-dest:exe:gc-stats -- +RTS -s -RTS runParseWithoutDestCopyRegion
-- cabal run -w $(pwd)/ghc@580d39a221/bin/ghc --allow-newer linear-dest:exe:gc-stats -- +RTS -s -RTS runParseWithDest

-- Profile memory usage with
-- cabal run -w $(pwd)/ghc@580d39a221/bin/ghc --allow-newer --enable-profiling --profiling-detail=late linear-dest:exe:gc-stats -- +RTS -p -N1 -RTS runParseWithoutDestForce && mv gc-stats.prof memory_without_dest_force.prof
-- cabal run -w $(pwd)/ghc@580d39a221/bin/ghc --allow-newer --enable-profiling --profiling-detail=late linear-dest:exe:gc-stats -- +RTS -p -N1 -RTS runParseWithoutDestCopyRegion && mv gc-stats.prof memory_without_dest_copy_region.prof
-- cabal run -w $(pwd)/ghc@580d39a221/bin/ghc --allow-newer --enable-profiling --profiling-detail=late linear-dest:exe:gc-stats -- +RTS -p -N1 -RTS runParseWithDest && mv gc-stats.prof memory_with_dest.prof

-- remove useless lines in profiling results with
-- .*?0\.0    0\.0     0\.0    0\.0\n
-- remove all lines with no individual contribution to alloc with
-- .*?([0-9]+\.[0-9])\s+?0\.0\s+?([0-9]+\.[0-9])\s+?([0-9]+\.[0-9])\n
-- remove all lines with no inherited contribution to alloc with
-- .*?([0-9]+\.[0-9])\s+?([0-9]+\.[0-9])\s+?([0-9]+\.[0-9])\s+?0\.0\n

main :: IO ()
main = do
  args <- getArgs
  !sampleData <- loadSampleData
  res <- case args of
    "runParseWithoutDestForce" : _ -> do
      let res = parseWithoutDest sampleData
      evaluate . force $ res
    "runParseWithoutDestCopyRegion" : _ -> do
      let res = parseWithoutDest sampleData
      compResInRegion <- compact res
      evaluate . getCompact $ compResInRegion
    "runParseWithDest" : _ -> do
      evaluate . parseWithDest $ sampleData
  putStrLn $ case res of
    Right _ -> "Done!"
    _ -> "Error!"
