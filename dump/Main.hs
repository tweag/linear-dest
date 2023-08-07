{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}

module Main (main) where

import System.Environment
import Compact.Pure
import Compact.Pure.SExpr
import Control.Functor.Linear ((<&>))
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import GHC.Exts
import Unsafe.Coerce (unsafeCoerceAddr)
import GHC.Compact (compact, getCompact)

-- run with
-- cabal run -w /home/thomas/tweag/ghc/_build/stage1/bin/ghc --allow-newer --ghc-options='-threaded -O2 -rtsopts' exe:dump -- +RTS -s -RTS runParseWithoutDest
-- cabal run -w /home/thomas/tweag/ghc/_build/stage1/bin/ghc --allow-newer --ghc-options='-threaded -O2 -rtsopts' exe:dump -- +RTS -s -RTS runParseWithDest

main :: IO ()
main = do
  args <- getArgs
  !sampleData <- evaluate =<< force <$> loadSampleData
  res <- case args of
    "runParseWithoutDest" : _ -> do
      let res = parseWithoutDest sampleData
      compResInRegion <- compact res
      evaluate . getCompact $ compResInRegion
    "runParseWithDest" : _ -> do
      evaluate . parseWithDest $ sampleData
  putStrLn $ case res of
    Right _ -> "Done!"
    _ -> "Error!"
