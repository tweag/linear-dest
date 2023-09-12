{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE KindSignatures #-}

module Main (main) where

import Test.Tasty.Bench
import Control.DeepSeq (force)
import Control.Exception (evaluate)

import qualified Map.Bench as Map
import qualified TreeTraversal.Bench as TreeTraversal
import qualified Prelude.Linear as Lin

-- run with
-- cabal bench -w /home/thomas/tweag/ghc/_build/stage1/bin/ghc --allow-newer --ghc-options='-threaded -O2 -rtsopts' linear-dest:bench:jflart --benchmark-options='+RTS -T -N1 -RTS'

loadListSampleData :: IO [Int]
loadListSampleData = return [1..100000]

listTransformer :: Int %1 -> Int
listTransformer x = 2 Lin.* x Lin.+ 1

main :: IO ()
main = do
  !listSampleData <- evaluate =<< force <$> loadListSampleData
  defaultMain
        [ Map.benchmark listSampleData listTransformer
        , Map.safety listSampleData listTransformer
        , TreeTraversal.safety
        ]
        
