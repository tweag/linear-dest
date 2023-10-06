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
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}

module TreeTraversal.Impl where

import Compact.Pure
import Control.Functor.Linear (Monad, return, (<&>), (>>=))
import Data.Kind (Type)
import GHC.Generics
import Prelude.Linear
import Queue.Impl
import qualified Prelude as NonLin

data BinTree a where
  Nil :: BinTree a
  Node :: a %1 -> (BinTree a) %1 -> (BinTree a) %1 -> BinTree a deriving (NonLin.Eq, Generic, NonLin.Show)

pattern Leaf x = Node x Nil Nil

mapAccumBFS :: forall a b s. (s -> a -> (s, b)) -> s -> BinTree a -> (s, BinTree b)
mapAccumBFS f s0 tree =
  (\(Ur (x, y)) -> (y, x)) . withRegion $
    \(_ :: Proxy r) token -> fromIncomplete $ alloc token <&>
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
