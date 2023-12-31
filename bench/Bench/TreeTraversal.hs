{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}

module Bench.TreeTraversal where

import Compact.Pure
import Control.Functor.Linear ((<&>))
import GHC.Generics
import Prelude.Linear
import Bench.Queue hiding (dataSets, impls)
import Data.Proxy (Proxy)
import Control.DeepSeq (NFData)
import qualified Prelude as NonLin
import qualified Data.Functor as NonLin
import Prelude (Functor, fmap, Applicative, pure, (<*>), return)
import Control.Exception (evaluate)
import Control.DeepSeq (force)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCaseInfo)
import Control.Monad.State.Lazy (runState, state)

data BinTree a where
  Nil :: BinTree a
  Node :: a %1 -> (BinTree a) %1 -> (BinTree a) %1 -> BinTree a deriving (NonLin.Eq, Generic, NonLin.Show, NFData)

pattern Leaf :: forall {a}. a -> BinTree a
pattern Leaf x = Node x Nil Nil

-- From "Phases in Software Architecture", Gibbons & al. 2023
--------------------------------------------------------------------------------

(⊗) :: Applicative m => m a -> m b -> m (a, b)
xs ⊗ ys = pure (,) <*> xs <*> ys

data Phases m a where
  Pure :: a → Phases m a
  Link :: (a → b → c) → m a → Phases m b → Phases m c

instance Functor (Phases m) where
  fmap f (Pure x) = Pure (f x)
  fmap f (Link g mx my) = Link (\x y -> f (g x y)) mx my

instance Applicative m ⇒ Applicative (Phases m) where
  pure x = Pure x
  Pure f <*> xs = NonLin.fmap f xs
  fs <*> Pure x = NonLin.fmap (\f → f x) fs
  Link f xs ys <*> Link g zs ws = Link h (xs ⊗ zs) (ys ⊗ ws) where h (x, z) (y, w) = (f x y) (g z w)

now :: Applicative m ⇒ m a → Phases m a
now xs = Link (curry fst) xs (Pure ())

later :: Applicative m ⇒ Phases m a → Phases m a
later xs = Link (curry snd) (pure ()) xs

phase :: Applicative m ⇒ Int → m a → Phases m a
phase 1 = now
phase i = later NonLin.. phase (i - 1)

runPhases :: Applicative m ⇒ Phases m a → m a
runPhases (Pure x) = pure x
runPhases (Link f xs ys) = pure f <*> xs <*> runPhases ys

bft' :: Applicative m ⇒ (a → m b) → BinTree a → Phases m (BinTree b)
bft' _ Nil = pure Nil
bft' f (Node x tl tr) = pure Node <*> now (f x) <*> later ((bft' f) tl) <*> later ((bft' f) tr)

mapPhasesBFS :: Applicative m ⇒ (a → m b) → BinTree a → m (BinTree b)
mapPhasesBFS f = runPhases NonLin.. bft' f

--------------------------------------------------------------------------------

mapAccumBFS :: forall a b s. (s -> a -> (s, b)) -> s -> BinTree a -> (BinTree b, s)
mapAccumBFS f s0 tree =
  unur . withRegion $
    \(_ :: Proxy r) token -> fromIncomplete $ alloc @r token <&>
      \dtree -> go s0 (singletonN (Ur tree, dtree))
  where
    go :: forall r. (Region r) => s -> NaiveQueue (Ur (BinTree a), Dest r (BinTree b)) %1 -> Ur s
    go s q = case dequeueN q of
      Nothing -> Ur s
      Just ((utree, dtree), q') -> case utree of
        Ur Nil -> dtree & fill @'Nil `lseq` go s q'
        Ur (Node x tl tr) -> case dtree & fill @'Node of
          (dr, dtl, dtr) ->
            let q'' = q' `enqueueN` (Ur tl, dtl) `enqueueN` (Ur tr, dtr)
                (s', r) = f s x
             in dr & fillLeaf r `lseq` go s' q''

--------------------------------------------------------------------------------

dataSets :: [(IO (BinTree ()), String)]
dataSets =
  [ (evaluate $ force (go 0 10), "2^10")
  , (evaluate $ force (go 0 13), "2^13")
  , (evaluate $ force (go 0 16), "2^16")
  , (evaluate $ force (go 0 19), "2^19")
  , (evaluate $ force (go 0 22), "2^22")
  ]
  where
      go :: Int -> Int -> BinTree ()
      go currentDepth maxDepth =
        if currentDepth >= maxDepth
          then Nil
          else Node () (go (currentDepth + 1) maxDepth) (go (currentDepth + 1) maxDepth)

dpsRelabel :: BinTree () -> (BinTree Int, Int)
dpsRelabel base = mapAccumBFS (\s _ -> (s + 1, s)) 0 base

phasesRelabel :: BinTree () -> (BinTree Int, Int)
phasesRelabel base = runState (mapPhasesBFS (\_ -> state (\s -> (s, s + 1))) base) 0

impls :: [(BinTree () -> (BinTree Int, Int), String, Bool)]
impls =
  [ (dpsRelabel, "dpsRelabel", False)
  , (phasesRelabel, "phasesRelabel", True)
  ]

extraSafety :: IO TestTree
extraSafety =
  return $ testGroup "extraSafety" $ impls NonLin.<&> \(impl, implName, _) ->
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
                return $ ""
