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

-- run with
-- cabal run -w /home/thomas/tweag/ghc/_build/stage1/bin/ghc --allow-newer linear-dest:exe:dump -- +RTS -s -RTS runParseWithoutDestForce
-- cabal run -w /home/thomas/tweag/ghc/_build/stage1/bin/ghc --allow-newer linear-dest:exe:dump -- +RTS -s -RTS runParseWithoutDestCopyRegion
-- cabal run -w /home/thomas/tweag/ghc/_build/stage1/bin/ghc --allow-newer linear-dest:exe:dump -- +RTS -s -RTS runParseWithDest

main :: IO ()
main = do
  args <- getArgs
  !sampleData <- evaluate =<< force <$> loadSampleData
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
