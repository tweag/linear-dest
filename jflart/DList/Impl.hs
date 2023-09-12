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

new :: forall r a. (RegionContext r) => DList r a
new = DList (alloc (getToken @r))

append :: forall r a. (RegionContext r) => DList r a %1 -> a -> DList r a
append (DList i) x =
  DList $ i <&> \dl -> case dl & fill @'(:) of
    (dh, dt) -> dh & fillLeaf x `lseq` dt

concat :: forall r a. (RegionContext r) => DList r a %1 -> DList r a %1 -> DList r a
concat (DList i1) (DList i2) = DList $ i1 <&> \dl -> dl & fillComp i2

toList :: forall r a. (RegionContext r) => DList r a %1 -> [a]
toList (DList i) = unur . fromReg $ i <&> \dl -> dl & fill @'[]
