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

import Prelude.Linear
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import TreeTraversal.Impl
import qualified Prelude as NonLin

safety :: TestTree
safety =
  testGroup
    "safety"
    [ testCaseInfo "mapAccumBFS" $ do
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
            experimental :: (BinTree Int, Int)
            experimental = mapAccumBFS (\s _ -> (s + 1, s)) 0 base
        assertEqual "same result" ref experimental
        NonLin.return $ show $ experimental
    ]
