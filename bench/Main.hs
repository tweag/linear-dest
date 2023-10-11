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
{-# OPTIONS_GHC -Wno-x-partial #-}

module Main (main) where

import System.Environment
import Test.Tasty.Bench
import qualified Bench.Map as Map
import qualified Bench.TreeTraversal as TreeTraversal
import qualified Bench.DList as DList
import qualified Bench.Queue as Queue
import qualified Bench.Parser as Parser
import qualified Bench.Utils as Utils

-- run with
-- cabal bench -w $(pwd)/ghc@580d39a221/bin/ghc --allow-newer linear-dest:bench:bench --benchmark-options='+RTS -T -N1 -RTS'
-- run in isolation with
-- cabal run -w $(pwd)/ghc@580d39a221/bin/ghc -v0 linear-dest:bench:bench -- -l | grep -P 'All\.[^\.]+\.benchmark\.' | while read -r name; do cabal run -w $(pwd)/ghc@580d39a221/bin/ghc -v0 linear-dest:bench:bench -- -p '$0 == "'"$name"'"' +RTS -T -N1 -RTS; done

launchImpl :: String -> IO ()
launchImpl s =
  let (_all, dotModuleName) = span (/= '.') s
      (moduleName, dotBenchmark) = span (/= '.') (tail dotModuleName)
      (_benchmark, dotImplSizeSpec) = span (/= '.') (tail dotBenchmark)
      implSizeSpec = tail dotImplSizeSpec
   in case (_all ++ "." ++ moduleName ++ "." ++ _benchmark) of
        "All.Map.benchmark" -> Utils.launchImpl implSizeSpec Map.impls Map.dataSets
        "All.TreeTraversal.benchmark" -> Utils.launchImpl implSizeSpec TreeTraversal.impls TreeTraversal.dataSets
        "All.DList.benchmark" -> Utils.launchImpl implSizeSpec DList.impls DList.dataSets
        "All.Queue.benchmark" -> Utils.launchImpl implSizeSpec Queue.impls Queue.dataSets
        "All.Parser.benchmark" -> Utils.launchImpl implSizeSpec Parser.impls Parser.dataSets
        _ -> launchAllBenchs

launchAllBenchs :: IO ()
launchAllBenchs = do
  mapBenchgroup <- bgroup "Map" <$> sequence [ Utils.benchImpls Map.impls Map.dataSets, Utils.safetySameAsFirstImpl Map.impls Map.dataSets ]
  treetraversalBenchgroup <- bgroup "TreeTraversal" <$> sequence [ Utils.benchImpls TreeTraversal.impls TreeTraversal.dataSets, Utils.safetySameAsFirstImpl TreeTraversal.impls TreeTraversal.dataSets, TreeTraversal.extraSafety ]
  dlistBenchgroup <- bgroup "DList" <$> sequence [ Utils.benchImpls DList.impls DList.dataSets, Utils.safetySameAsFirstImpl DList.impls DList.dataSets ]
  queueBenchgroup <- bgroup "Queue" <$> sequence [ Utils.benchImpls Queue.impls Queue.dataSets, Utils.safetySameAsFirstImpl Queue.impls Queue.dataSets ]
  parserBenchgroup <- bgroup "Parser" <$> sequence [ Utils.benchImpls Parser.impls Parser.dataSets, Utils.safetySameAsFirstImpl Parser.impls Parser.dataSets, Parser.extraSafety ]
  defaultMain
    [ mapBenchgroup,
      treetraversalBenchgroup,
      dlistBenchgroup,
      queueBenchgroup,
      parserBenchgroup
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    s : _ -> launchImpl s
    _ -> launchAllBenchs
