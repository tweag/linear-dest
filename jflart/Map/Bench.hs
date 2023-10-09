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
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Map.Bench (benchmark, safety, getBenchgroup) where

import Compact.Pure
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Data.Functor ((<&>))
import GHC.Compact (compact, getCompact)
import Map.Impl
import Prelude.Linear (unur, (+), (*))
import Prelude hiding ((+), (*))
import qualified Control.Functor.Linear as Lin
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Bench
import Test.Tasty.HUnit
import Data.Proxy (Proxy)

loadListData :: IO [Int]
loadListData = evaluate =<< force <$> return [1 .. 100000]

listTransformer :: Int %1 -> Int
listTransformer x = 2 * x + 1

safety :: forall a b. (Eq b, Show b) => [a] -> (a %1 -> b) -> TestTree
safety sampleData f =
  testGroup "safety" $
    destImpls <&> \(impl, implName) ->
      testCaseInfo ("mapL and " ++ implName ++ " give the same result") $ do
        let ref = mapL f sampleData
            experimental = unur (withRegion (\(_ :: Proxy r) t -> fromIncomplete_ (alloc @r t Lin.<&> \d -> impl f sampleData d)))
        assertEqual "same result" ref experimental
        return $ show $ take (min 10 (length experimental)) $ experimental

benchmark :: forall a b. (NFData b) => [a] -> (a %1 -> b) -> Benchmark
benchmark sampleData f =
  bgroup
    "map implementations"
    ( ( concat $
          nonDestImpls <&> \(impl, implName) ->
            [ bench (implName ++ " (with force)") $ (flip whnfAppIO) sampleData $ \sampleData -> do
                evaluate $ force $ impl @a @b f sampleData,
              bench (implName ++ " (with copy into region)") $ (flip whnfAppIO) sampleData $ \sampleData -> do
                resInRegion <- compact $ impl @a @b f sampleData
                evaluate $ getCompact $ resInRegion
            ]
      )
        ++ ( destImpls <&> \(impl, implName) ->
               bench implName $ (flip whnfAppIO) sampleData $ \sampleData -> do
                 evaluate $ unur (withRegion (\(_ :: Proxy r) t -> fromIncomplete_ (alloc @r t Lin.<&> \d -> impl f sampleData d)))
           )
    )

getBenchgroup :: IO Benchmark
getBenchgroup = do
  !listData <- loadListData
  return $ bgroup "map"
    [ benchmark listData listTransformer,
      safety listData listTransformer
    ]
