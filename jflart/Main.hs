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
import qualified Parser.Bench as Parser

-- run with
-- cabal bench -w $(pwd)/ghc@580d39a221/bin/ghc --allow-newer linear-dest:bench:jflart --benchmark-options='+RTS -T -N1 -RTS'
-- run in isolation with
-- cabal run -w $(pwd)/ghc@580d39a221/bin/ghc -v0 linear-dest:bench:jflart -- -l | grep -P 'All\.[^\.]+\.benchmark\.' | while read -r name; do cabal run -w $(pwd)/ghc@580d39a221/bin/ghc -v0 linear-dest:bench:jflart -- -p '$0 == "'"$name"'"' +RTS -T -N1 -RTS; done

main :: IO ()
main = do
  !queueBenchgroup <- Queue.getBenchgroup
  !mapBenchgroup <- Map.getBenchgroup
  !dlistBenchgroup <- DList.getBenchgroup
  !treeTraversalBenchgroup <- TreeTraversal.getBenchgroup
  !parserBenchgroup <- Parser.getBenchgroup
  defaultMain
    [ mapBenchgroup,
      dlistBenchgroup,
      queueBenchgroup,
      treeTraversalBenchgroup,
      parserBenchgroup
    ]
