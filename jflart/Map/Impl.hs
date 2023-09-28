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

module Map.Impl where

import Compact.Pure
import Control.Functor.Linear ((<&>))
import Data.Kind (Type)
import Prelude.Linear

nonDestImpls :: [(forall a b. (a %1 -> b) -> [a] %1 -> [b], String)]
nonDestImpls =
  [ (mapL, "mapL"),
    (mapS, "mapS"),
    (mapSH, "mapSH"),
    (mapST, "mapST"),
    (mapS, "mapS"),
    (mapTRL, "mapTRL"),
    (mapTRS, "mapTRS"),
    (mapTRSH, "mapTRSH"),
    (mapTRST, "mapTRST")
  ]

destImpls :: [(forall (r :: Type) a b. (RegionContext r) => RegionToken r %1 -> (a %1 -> b) -> [a] -> Ur [b], String)]
destImpls =
  [ (\(t :: RegionToken r) -> consume t `lseq` mapDestTRL @r, "mapDestTRL"),
    (\(t :: RegionToken r) -> consume t `lseq` mapDestTRS @r, "mapDestTRS"),
    (\(t :: RegionToken r) -> consume t `lseq` mapDestFL @r, "mapDestFL"),
    (\(t :: RegionToken r) -> consume t `lseq` mapDestFLS @r, "mapDestFLS"),
    (\(t :: RegionToken r) -> consume t `lseq` mapDestFSL @r, "mapDestFSL"),
    (\(t :: RegionToken r) -> consume t `lseq` mapDestFS @r, "mapDestFS")
  ]
  where

mapL :: forall a b. (a %1 -> b) -> [a] %1 -> [b]
mapL _ [] = []
mapL f (x : xs) = (f x) : (mapL f xs)

mapS :: forall a b. (a %1 -> b) -> [a] %1 -> [b]
mapS _ [] = []
mapS f (x : xs) =
  let !r = f x
      !tail = mapS f xs
   in r : tail

mapSH :: forall a b. (a %1 -> b) -> [a] %1 -> [b]
mapSH _ [] = []
mapSH f (x : xs) =
  let !r = f x
   in r : (mapSH f xs)

mapST :: forall a b. (a %1 -> b) -> [a] %1 -> [b]
mapST _ [] = []
mapST f (x : xs) =
  let !tail = mapST f xs
   in (f x) : tail

mapTRL :: forall a b. (a %1 -> b) -> [a] %1 -> [b]
mapTRL f = go []
  where
    go acc [] = reverse acc
    go acc (x : xs) = go ((f x) : acc) xs

mapTRS :: forall a b. (a %1 -> b) -> [a] %1 -> [b]
mapTRS f = go []
  where
    go acc [] = reverse acc
    go acc (x : xs) =
      let !r = f x
          !cons = r : acc
       in go cons xs

mapTRSH :: forall a b. (a %1 -> b) -> [a] %1 -> [b]
mapTRSH f = go []
  where
    go acc [] = reverse acc
    go acc (x : xs) =
      let !r = f x
       in go (r : acc) xs

mapTRST :: forall a b. (a %1 -> b) -> [a] %1 -> [b]
mapTRST f = go []
  where
    go acc [] = reverse acc
    go acc (x : xs) =
      let !cons = (f x) : acc
       in go cons xs

mapDestTRL :: forall (r :: Type) a b. (RegionContext r) => (a %1 -> b) -> [a] -> Ur [b]
mapDestTRL f l =
  fromRegion $ alloc (getToken @r) <&> go l
  where
    go [] dl = dl & fill @'[]
    go (x : xs) dl = case dl & fill @'(:) of
      (dh, dt) -> dh & fillLeaf (f x) `lseq` go xs dt

mapDestTRS :: forall (r :: Type) a b. (RegionContext r) => (a %1 -> b) -> [a] -> Ur [b]
mapDestTRS f l =
  fromRegion $ alloc (getToken @r) <&> go l
  where
    go [] dl = dl & fill @'[]
    go (x : xs) dl = case dl & fill @'(:) of
      (dh, dt) -> let !r = f x in dh & fillLeaf r `lseq` go xs dt

mapDestFL :: forall (r :: Type) a b. (RegionContext r) => (a %1 -> b) -> [a] -> Ur [b]
mapDestFL f l =
  fromRegion $ alloc (getToken @r) <&> (\dl -> foldl_ fillConsF dl l) <&> (\dl -> fill @'[] dl)
  where
    fillConsF dl x = case dl & fill @'(:) of
      (dh, dt) -> dh & fillLeaf (f x) `lseq` dt
    foldl_ :: forall a b. (a %1 -> b -> a) -> a %1 -> [b] -> a
    foldl_ _ s [] = s
    foldl_ f s (x : xs) = foldl_ f (f s x) xs

mapDestFSL :: forall (r :: Type) a b. (RegionContext r) => (a %1 -> b) -> [a] -> Ur [b]
mapDestFSL f l =
  fromRegion $ alloc (getToken @r) <&> (\dl -> foldl_ fillConsF dl l) <&> (\dl -> fill @'[] dl)
  where
    fillConsF dl x = case dl & fill @'(:) of
      (dh, dt) -> dh & fillLeaf (f x) `lseq` dt
    foldl_ :: forall a b. (a %1 -> b -> a) -> a %1 -> [b] -> a
    foldl_ _ s [] = s
    foldl_ f s (x : xs) = let !r = (f s x) in foldl_ f r xs

mapDestFLS :: forall (r :: Type) a b. (RegionContext r) => (a %1 -> b) -> [a] -> Ur [b]
mapDestFLS f l =
  fromRegion $ alloc (getToken @r) <&> (\dl -> foldl_ fillConsF dl l) <&> (\dl -> fill @'[] dl)
  where
    fillConsF dl x = case dl & fill @'(:) of
      (dh, dt) -> let !r = f x in dh & fillLeaf r `lseq` dt
    foldl_ :: forall a b. (a %1 -> b -> a) -> a %1 -> [b] -> a
    foldl_ _ s [] = s
    foldl_ f s (x : xs) = foldl_ f (f s x) xs

mapDestFS :: forall (r :: Type) a b. (RegionContext r) => (a %1 -> b) -> [a] -> Ur [b]
mapDestFS f l =
  fromRegion $ alloc (getToken @r) <&> (\dl -> foldl_ fillConsF dl l) <&> (\dl -> fill @'[] dl)
  where
    fillConsF dl x = case dl & fill @'(:) of
      (dh, dt) -> let !r = f x in dh & fillLeaf r `lseq` dt
    foldl_ :: forall a b. (a %1 -> b -> a) -> a %1 -> [b] -> a
    foldl_ _ s [] = s
    foldl_ f s (x : xs) = let !r = (f s x) in foldl_ f r xs
