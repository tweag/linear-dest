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
      (_benchmark, dotImplSpec) = span (/= '.') (tail dotBenchmark)
      implSpec = tail dotImplSpec
   in case (_all ++ "." ++ moduleName ++ "." ++ _benchmark) of
        "All.Map.benchmark" -> Utils.launchImpl implSpec Map.impls Map.loadBenchData
        "All.TreeTraversal.benchmark" -> Utils.launchImpl implSpec TreeTraversal.impls TreeTraversal.loadBenchData
        "All.DList.benchmark" -> Utils.launchImpl implSpec DList.impls DList.loadBenchData
        "All.Queue.benchmark" -> Utils.launchImpl implSpec Queue.impls Queue.loadBenchData
        "All.Parser.benchmark" -> Utils.launchImpl implSpec Parser.impls Parser.loadBenchData
        _ -> launchAllBenchs

launchAllBenchs :: IO ()
launchAllBenchs = do
  mapBenchgroup <- bgroup "Map" <$> sequence [ Utils.benchImpls Map.impls Map.loadBenchData, Utils.safetySameAsFirstImpl Map.impls Map.loadBenchData ]
  treetraversalBenchgroup <- bgroup "TreeTraversal" <$> sequence [ Utils.benchImpls TreeTraversal.impls TreeTraversal.loadBenchData, Utils.safetySameAsFirstImpl TreeTraversal.impls TreeTraversal.loadBenchData, TreeTraversal.extraSafety ]
  dlistBenchgroup <- bgroup "DList" <$> sequence [ Utils.benchImpls DList.impls DList.loadBenchData, Utils.safetySameAsFirstImpl DList.impls DList.loadBenchData ]
  queueBenchgroup <- bgroup "Queue" <$> sequence [ Utils.benchImpls Queue.impls Queue.loadBenchData, Utils.safetySameAsFirstImpl Queue.impls Queue.loadBenchData ]
  parserBenchgroup <- bgroup "Parser" <$> sequence [ Utils.benchImpls Parser.impls Parser.loadBenchData, Utils.safetySameAsFirstImpl Parser.impls Parser.loadBenchData, Parser.extraSafety ]
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
