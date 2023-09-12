{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import qualified Map.Bench as Map
import qualified Prelude.Linear as Lin
import Test.Tasty.Bench
import qualified TreeTraversal.Bench as TreeTraversal

-- run with
-- cabal bench -w /home/thomas/tweag/ghc/_build/stage1/bin/ghc --allow-newer --ghc-options='-threaded -O2 -rtsopts' linear-dest:bench:jflart --benchmark-options='+RTS -T -N1 -RTS'

loadListSampleData :: IO [Int]
loadListSampleData = return [1 .. 100000]

listTransformer :: Int %1 -> Int
listTransformer x = 2 Lin.* x Lin.+ 1

main :: IO ()
main = do
  !listSampleData <- evaluate =<< force <$> loadListSampleData
  defaultMain
    [ Map.benchmark listSampleData listTransformer,
      Map.safety listSampleData listTransformer,
      TreeTraversal.safety
    ]
