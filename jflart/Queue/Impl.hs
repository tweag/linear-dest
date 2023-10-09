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

module Queue.Impl where

import Compact.Pure
import DList.Impl (DList (DList))
import qualified DList.Impl as DList
import Prelude.Linear

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
    (x : xs) -> Just (x, NaiveQueue xs [])
  (x : xs) -> Just (x, NaiveQueue xs r)

data Queue r a = Queue [a] (DList r a)

new :: forall r a. (Region r) => Token %1 -> Queue r a
new token = Queue [] (DList.new @r token)

singleton :: forall r a. (Region r) => Token %1 -> a -> Queue r a
singleton token x = Queue [x] (DList.new @r token)

toList :: forall r a. (Region r) => Queue r a %1 -> [a]
toList (Queue l dl) = l ++ DList.toList dl

enqueue :: forall r a. (Region r) => Queue r a %1 -> a -> Queue r a
enqueue (Queue l dl) x = Queue l (DList.append dl x)

dequeue :: forall r a. (Region r) => Queue r a %1 -> Maybe (a, Queue r a)
dequeue (Queue l (DList i)) = case l of
  [] -> let !(i', token) = piggyback i in case DList.toList (DList i') of
    [] -> consume token `lseq` Nothing
    (x : xs) -> Just (x, Queue xs (DList.new @r token))
  (x : xs) -> Just (x, Queue xs (DList i))
