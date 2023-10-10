{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-x-partial #-}

module TreeTraversal.Bench (benchmark, safety, getBenchgroup) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Functor ((<&>))
import TreeTraversal.Impl
import Prelude.Linear
import Prelude ((=<<), return, (<$>))
import Control.Monad.State.Lazy (state, runState)
import GHC.Compact (compact, getCompact)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Bench
import Test.Tasty.HUnit

loadTreeData :: IO (BinTree ())
loadTreeData = evaluate =<< force <$>
  let maxDepth :: Int
      maxDepth = 16
      go currentDepth =
        if currentDepth >= maxDepth
          then Nil
          else Node () (go (currentDepth + 1)) (go (currentDepth + 1))
   in return (go 0)

dpsRelabel :: BinTree () -> (BinTree Int, Int)
dpsRelabel base = mapAccumBFS (\s _ -> (s + 1, s)) 0 base

phasesRelabel :: BinTree () -> (BinTree Int, Int)
phasesRelabel base = runState (mapPhasesBFS (\_ -> state (\s -> (s, s + 1))) base) 0

impls :: [(BinTree () -> (BinTree Int, Int), String, Bool)]
impls =
  [ (dpsRelabel, "dpsRelabel", False)
  , (phasesRelabel, "phasesRelabel", True)
  ]

safety :: BinTree () -> TestTree
safety sampleData =
  testGroup "safety" $
    ((tail impls) <&> \(impl, implName, _) ->
      testCaseInfo ("relabel using destinations and " ++ implName ++ " give the same result") $ do
        let expected = dpsRelabel sampleData
            actual = impl sampleData
        assertEqual "same result" expected actual
        return $ "<too large to display>")
      ++ (
        impls <&> \(impl, implName, _) ->
          testCaseInfo (implName ++ " give the good result on a small example") $ do
            let expected :: (BinTree Int, Int)
                expected =
                  ( Node
                      0
                      (Node 1 (Leaf 3) (Leaf 4))
                      (Node 2 (Leaf 5) Nil),
                    6
                  )
                base :: BinTree ()
                base =
                  Node
                    ()
                    (Node () (Leaf ()) (Leaf ()))
                    (Node () (Leaf ()) Nil)
                actual = impl base
            assertEqual "same result" expected actual
            return $ show $ actual)

benchmark :: BinTree () -> Benchmark
benchmark sampleData =
  bgroup "benchmark" $
    concat $ impls <&> \(impl, implName, isLazy) -> if isLazy
      then
        [ bench (implName ++ " (with force)") $ (flip whnfAppIO) sampleData $ \sampleData -> evaluate $ force $ impl sampleData,
          bench (implName ++ " (with copy into region)") $ (flip whnfAppIO) sampleData $ \sampleData -> do
                resInRegion <- compact $ impl sampleData
                evaluate $ getCompact $ resInRegion
        ]
      else
        [ bench implName $ (flip whnfAppIO) sampleData $ \sampleData -> evaluate $ impl sampleData ]

getBenchgroup :: IO Benchmark
getBenchgroup = do
  !treeData <- loadTreeData
  return $ bgroup "breadth-first-tree-traversal"
    [ benchmark treeData,
      safety treeData
    ]
