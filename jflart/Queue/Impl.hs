{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}

module Queue.Impl where

import Prelude.Linear
import Compact.Pure
import Control.Functor.Linear ((<&>))
import Data.Kind (Type)
import DList.Impl (DList)
import qualified DList.Impl as DList

data NaiveQueue a = NaiveQueue [a] [a]

newN :: NaiveQueue a
newN = NaiveQueue [] []

singletonN :: a %1 -> NaiveQueue a
singletonN x = NaiveQueue [x] []

toListN :: NaiveQueue a %1 -> [a]
toListN (NaiveQueue l r) = l ++ reverse r

enqueueN :: NaiveQueue a %1 -> a %1 -> NaiveQueue a
enqueueN (NaiveQueue l r) x = NaiveQueue l (x : r)

dequeueN :: NaiveQueue a %1 -> Maybe (a, NaiveQueue a)
dequeueN (NaiveQueue l r) = case l of
    [] -> case reverse r of
        [] -> Nothing
        (x:xs) -> Just (x, NaiveQueue xs [])
    (x:xs) -> Just (x, NaiveQueue xs r)

data Queue r a = Queue [a] (DList r a)

new :: forall r a. RegionContext r => Queue r a
new = Queue [] (DList.new @r)

singleton :: forall r a. RegionContext r => a -> Queue r a
singleton x = Queue [x] (DList.new @r)

toList :: forall r a. RegionContext r => Queue r a %1 -> [a]
toList (Queue l dl) = l ++ DList.toList dl

enqueue :: forall r a. RegionContext r => Queue r a %1 -> a -> Queue r a
enqueue (Queue l dl) x = Queue l (DList.append dl x)

dequeue :: forall r a. RegionContext r => Queue r a %1 -> Maybe (a, Queue r a)
dequeue (Queue l dl) = case l of
    [] -> case DList.toList dl of
        [] -> Nothing
        (x:xs) -> Just (x, Queue xs (DList.new @r))
    (x:xs) -> Just (x, Queue xs dl)
