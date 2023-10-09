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

import Test.Tasty.Bench
import qualified Map.Bench as Map
import qualified TreeTraversal.Bench as TreeTraversal
import qualified DList.Bench as DList
import qualified Queue.Bench as Queue

-- run with
-- cabal bench -w /home/thomas/tweag/ghc/_build/stage1/bin/ghc --allow-newer linear-dest:bench:jflart --benchmark-options='+RTS -T -N1 -RTS'

main :: IO ()
main = do
  !queueBenchgroup <- Queue.getBenchgroup
  !mapBenchgroup <- Map.getBenchgroup
  !dlistBenchgroup <- DList.getBenchgroup
  !treeTraversalBenchgroup <- TreeTraversal.getBenchgroup
  defaultMain
    [ mapBenchgroup,
      dlistBenchgroup,
      queueBenchgroup,
      treeTraversalBenchgroup
    ]
