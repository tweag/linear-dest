{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-type-defaults #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}

module Bench.Queue where

import Compact.Pure
import qualified Bench.DList as DList
import Bench.DList (DListN(..), DList(..))
import Prelude.Linear hiding ((+), (*), (<))
import Prelude (return, (+), (*), (<))
import Data.Word
import Data.Proxy (Proxy)

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

data QueueF a = QueueF [a] (DListN a)

newF :: forall a. QueueF a
newF = QueueF [] DList.newN

singletonF :: forall a. a %1 -> QueueF a
singletonF x = QueueF [x] DList.newN

toListF :: forall a. QueueF a %1 -> [a]
toListF (QueueF l dl) = l ++ DList.toListN dl

enqueueF :: forall a. QueueF a %1 -> a %1 -> QueueF a
enqueueF (QueueF l dl) x = QueueF l (DList.appendN dl x)

dequeueF :: forall a. QueueF a %1 -> Maybe (a, QueueF a)
dequeueF (QueueF l dl) = case l of
  [] -> case DList.toListN dl of
    [] -> Nothing
    (x : xs) -> Just (x, QueueF xs DList.newN)
  (x : xs) -> Just (x, QueueF xs dl)

data Queue r a where
  Queue :: [a] -> DList r a %1 -> Queue r a

new :: forall r a. (Region r) => Token %1 -> Queue r a
new token = Queue [] (DList.new @r token)

singleton :: forall r a. (Region r) => Token %1 -> a -> Queue r a
singleton token x = Queue [x] (DList.new @r token)

toUList :: forall r a. (Region r) => Queue r a %1 -> Ur [a]
toUList (Queue l dl) = case DList.toUList dl of
  Ur l' -> Ur $ l ++ l'

toList :: forall r a. (Region r) => Queue r a %1 -> [a]
toList = unur . toUList

enqueue :: forall r a. (Region r) => Queue r a %1 -> a -> Queue r a
enqueue (Queue l dl) x = Queue l (DList.append dl x)

dequeue :: forall r a. (Region r) => Queue r a %1 -> Maybe (Ur a, Queue r a)
dequeue (Queue l (DList i)) = case l of
  [] -> let !(i', token) = piggyback i in case DList.toUList (DList i') of
    Ur [] -> consume token `lseq` Nothing
    Ur (x : xs) -> Just (Ur x, Queue xs (DList.new @r token))
  (x : xs) -> Just (Ur x, Queue xs (DList i))

-------------------------------------------------------------------------------

dataSets :: [(IO Word64, String)]
dataSets =
  [ (return $ 2^10, "2^10")
  , (return $ 2^13, "2^13")
  , (return $ 2^16, "2^16")
  , (return $ 2^19, "2^19")
  , (return $ 2^22, "2^22")
  ]

naiveImpl :: Word64 -> Word64
naiveImpl limit = go 0 (singletonN 1)
  where
    go sum q = case dequeueN q of
      Nothing -> sum
      Just (x, q') -> go (sum + x) q'' where
        q'' = if x < limit
                then q' `enqueueN` (2 * x) `enqueueN` (2 * x + 1)
                else q'

funcImpl :: Word64 -> Word64
funcImpl limit = go 0 (singletonF 1)
  where
    go sum q = case dequeueF q of
      Nothing -> sum
      Just (x, q') -> go (sum + x) q'' where
        q'' = if x < limit
                then q' `enqueueF` (2 * x) `enqueueF` (2 * x + 1)
                else q'

destImpl :: Word64 -> Word64
destImpl limit = unur (withRegion (\(_ :: Proxy r) t -> go 0 (singleton @r t 1)))
  where
    go :: Region r => Word64 -> Queue r Word64 %1 -> Ur Word64
    go sum q = case dequeue q of
      Nothing -> Ur sum
      Just (Ur x, q') -> go (sum + x) q'' where
        q'' = if x < limit
                then q' `enqueue` (2 * x) `enqueue` (2 * x + 1)
                else q'

impls :: [(Word64 -> Word64, String, Bool)]
impls =
  [ (naiveImpl, "naiveImpl", False)
  , (funcImpl, "funcImpl", False)
  , (destImpl, "destImpl", False)
  ]
