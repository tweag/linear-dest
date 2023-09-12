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
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module TreeTraversal.Bench (safety) where

import Compact.Pure (RegionToken, withRegion)
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Control.Functor.Linear
import GHC.Compact (compact, getCompact)
import Prelude.Linear
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Bench
import Test.Tasty.HUnit
import TreeTraversal.Impl
import qualified Prelude as NonLin

safety :: TestTree
safety =
  testGroup
    "safety"
    [ testCaseInfo "mapMBreadth" $ do
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
            setNodeNbAndIncr :: () %1 -> State Int Int
            setNodeNbAndIncr () =
              get >>= \c ->
                let !(c1, c2) = dup2 c
                 in put (c1 + 1) >>= \() ->
                      return c2
            experimental = runState (mapMBreadth setNodeNbAndIncr base) 0
            experimental' :: (BinTree Int, Int)
            experimental' = let !(u, c) = experimental in (unur u, c)
        assertEqual "same result" ref experimental'
        NonLin.return $ show $ experimental'
    ]
