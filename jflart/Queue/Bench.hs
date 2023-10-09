{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-x-partial #-}

module Queue.Bench (benchmark, safety, getBenchgroup) where

import Compact.Pure
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Functor ((<&>))
import Queue.Impl
import Prelude.Linear (unur, move, Ur (Ur))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Bench
import Test.Tasty.HUnit
import Data.Proxy (Proxy)
import Data.Bits
import Data.Word

limit :: Word64
limit = 2 `shiftL` 16

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
destImpl limit = unur (withRegion (\(_ :: Proxy r) t -> let r = go 0 (singleton @r t (Ur 1)) in move r))
  where
    go :: Region r => Word64 -> Queue r (Ur Word64) %1 -> Word64
    go sum q = case dequeue q of
      Nothing -> sum
      Just (Ur x, q') -> go (sum + x) q'' where
        q'' = if x < limit
                then q' `enqueue` Ur (2 * x) `enqueue` Ur (2 * x + 1)
                else q'

impls :: [(Word64 -> Word64, String)]
impls =
  [ (naiveImpl, "naive functional queue")
  , (funcImpl, "queue backed by difference lists using functions")
  , (destImpl, "queue backed by difference lists using destinations")
  ]

safety :: Word64 -> TestTree
safety sampleData =
  testGroup "safety" $
     (tail impls) <&> \(impl, implName) ->
      testCaseInfo ("naive functional queue and " ++ implName ++ " give the same result") $ do
        let ref = naiveImpl sampleData
            experimental = impl sampleData
        assertEqual "same result" ref experimental
        return $ show experimental

benchmark :: Word64 -> Benchmark
benchmark sampleData =
  bgroup "queue implementations" $
    impls <&> \(impl, implName) -> bench implName $ (flip whnfAppIO) sampleData $ \sampleData -> do
      evaluate $ force $ impl sampleData

getBenchgroup :: IO Benchmark
getBenchgroup = do
  return $ bgroup "queue"
    [ benchmark limit,
      safety limit
    ]
