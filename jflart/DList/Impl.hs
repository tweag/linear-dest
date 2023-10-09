{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}

module DList.Impl where

import Compact.Pure
import Control.Functor.Linear ((<&>))
import Prelude.Linear

newtype DList r a = DList (Incomplete r [a] (Dest r [a]))

new :: forall r a. (Region r) => Token %1 -> DList r a
new token = DList (alloc @r token)

append :: forall r a. (Region r) => DList r a %1 -> a -> DList r a
append (DList i) x =
  DList $ i <&> \dl -> case dl & fill @'(:) of
    (dh, dt) -> dh & fillLeaf x `lseq` dt

concat :: forall r a. (Region r) => DList r a %1 -> DList r a %1 -> DList r a
concat (DList i1) (DList i2) = DList $ i1 <&> \dl -> dl & fillComp i2

toList :: forall r a. (Region r) => DList r a %1 -> [a]
toList (DList i) = unur $ fromIncomplete_ $ i <&> \dl -> dl & fill @'[]

toUList :: forall r a. (Region r) => DList r a %1 -> Ur [a]
toUList (DList i) = fromIncomplete_ $ i <&> \dl -> dl & fill @'[]

fromList :: forall r a. (Region r) => Token %1 -> [a] -> DList r a
fromList token l = DList (alloc @r token <&> \d -> go d l) where
  go dl [] = dl
  go dl (x:xs) = case dl & fill @'(:) of (dh, dt) -> dh & fillLeaf x `lseq` go dt xs

newtype DListN a = DListN ([a] %1 -> [a])

newN :: forall a. DListN a
newN = DListN (\ys -> ys)

appendN :: forall a. DListN a %1 -> a %1 -> DListN a
appendN (DListN f) x =
  DListN $ \ys -> f ([x] ++ ys)

concatN :: forall a. DListN a %1 -> DListN a %1 -> DListN a
concatN (DListN f1) (DListN f2) = DListN $ f1 . f2

toListN :: forall a. DListN a %1 -> [a]
toListN (DListN f) = f []

fromListN :: forall a. [a] %1 -> DListN a
fromListN xs = DListN $ \ys -> xs ++ ys
