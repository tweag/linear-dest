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
  [ (dpsRelabel, "relabel using destinations", False)
  , (phasesRelabel, "relabel using phases applicative", True)
  ]

safety :: BinTree () -> TestTree
safety sampleData =
  testGroup "safety" $
    ((tail impls) <&> \(impl, implName, _) ->
      testCaseInfo ("relabel using destinations and " ++ implName ++ " give the same result") $ do
        let ref = dpsRelabel sampleData
            experimental = impl sampleData
        assertEqual "same result" ref experimental
        return $ show "<too large to display>")
      ++ (
        impls <&> \(impl, implName, _) ->
          testCaseInfo (implName ++ " give the good result on a small example") $ do
            let ref :: (BinTree Int, Int)
                ref =
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
                experimental = impl base
            assertEqual "same result" ref experimental
            return $ show $ experimental)

benchmark :: BinTree () -> Benchmark
benchmark sampleData =
  bgroup "breadth-first tree traversal implementations" $
    impls <&> \(impl, implName, isLazy) -> bench implName $ (flip whnfAppIO) sampleData $ \sampleData -> do
      evaluate $ (if isLazy then force else id) $ impl sampleData

getBenchgroup :: IO Benchmark
getBenchgroup = do
  !treeData <- loadTreeData
  return $ bgroup "breadth-first tree traversal"
    [ benchmark treeData,
      safety treeData
    ]
