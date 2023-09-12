{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module TreeTraversal.Bench (safety) where

import GHC.Compact (compact, getCompact)
import Test.Tasty.Bench
import Control.DeepSeq (force, NFData)
import Control.Exception (evaluate)
import Test.Tasty.HUnit
import Test.Tasty (testGroup, TestTree)
import Prelude.Linear
import Control.Functor.Linear
import qualified Prelude as NonLin

import Compact.Pure (RegionToken, withRegion)

import TreeTraversal.Impl

safety :: TestTree
safety =
  testGroup "safety" [
    testCaseInfo "mapMBreadth" $ do
      let ref :: (BinTree Int, Int)
          ref = ( Node 0
                    (Node 1 (Leaf 3) (Leaf 4))
                    (Node 2 (Leaf 5) Nil) , 6)
          base :: BinTree ()
          base = Node ()
                    (Node () (Leaf ()) (Leaf ()))
                    (Node () (Leaf ()) Nil)
          setNodeNbAndIncr :: () %1 -> State Int Int
          setNodeNbAndIncr () =
            get >>= \c ->
                let !(c1, c2) = dup2 c in
                put (c1 + 1) >>= \() ->
                    return c2
          experimental = runState (mapMBreadth setNodeNbAndIncr base) 0
          experimental' :: (BinTree Int, Int)
          experimental' = let !(u, c) = experimental in (unur u, c)
      assertEqual "same result" ref experimental'
      NonLin.return $ show $ experimental'
  ]
