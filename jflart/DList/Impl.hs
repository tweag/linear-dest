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
toList (DList i) = unur . fromIncomplete_ $ i <&> \dl -> dl & fill @'[]

fromList :: forall r a. (Region r) => Token %1 -> [a] -> DList r a
fromList token l = DList (alloc @r token <&> \d -> go d l) where
  go dl [] = dl
  go dl (x:xs) = case dl & fill @'(:) of (dh, dt) -> dh & fillLeaf x `lseq` go dt xs

-- newtype DListN a = DList ([a] -> [a])

-- newN :: forall a. DListN a
-- newN = DList (alloc (getToken @r))

-- appendN :: forall r a. (Region r) => DList r a %1 -> a -> DList r a
-- appendN (DList i) x =
--   DList $ i <&> \dl -> case dl & fill @'(:) of
--     (dh, dt) -> dh & fillLeaf x `lseq` dt

-- concatN :: forall r a. (Region r) => DList r a %1 -> DList r a %1 -> DList r a
-- concat (DList i1) (DList i2) = DList $ i1 <&> \dl -> dl & fillComp i2

-- toListN :: forall r a. (Region r) => DList r a %1 -> [a]
-- toListN (DList i) = unur . fromIncomplete_ $ i <&> \dl -> dl & fill @'[]