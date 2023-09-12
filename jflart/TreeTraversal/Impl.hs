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

data BinTree a = Nil | Node a (BinTree a) (BinTree a) deriving (NonLin.Eq, Generic, NonLin.Show)

pattern Leaf x = Node x Nil Nil

mapMBreadth :: forall a b m. (Monad m) => (a %1 -> m b) -> BinTree a -> m (Ur (BinTree b))
mapMBreadth f tree =
  withRegionM $ \(token :: RegionToken r) -> fromRegM $ alloc token <&> \dtree -> go (singletonN (tree, dtree))
  where
    go :: forall r. (RegionContext r) => NaiveQueue (BinTree a, Dest r (BinTree b)) %1 -> m ()
    go q = case dequeueN q of
      Nothing -> return ()
      Just ((tree, dtree), q') -> case tree of
        Nil -> dtree & fill @'Nil `lseq` go q'
        Node x tl tr -> case dtree & fill @'Node of
          (dr, dtl, dtr) ->
            let q'' = q' `enqueueN` (tl, dtl) `enqueueN` (tr, dtr)
             in f x >>= \r -> dr & fillLeaf r `lseq` go q''
