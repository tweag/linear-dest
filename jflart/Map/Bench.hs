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
import Data.Kind (Type)

loadListData :: IO [Int]
loadListData = evaluate =<< force <$> return [1 .. 100000]

listTransformer :: Int %1 -> Int
listTransformer x = 2 * x + 1

nonDestImpls :: [(forall a b. (a %1 -> b) -> [a] %1 -> [b], String)]
nonDestImpls =
  [ (mapL, "mapL"),
    (mapS, "mapS"),
    (mapSH, "mapSH"),
    (mapST, "mapST"),
    (mapTRL, "mapTRL"),
    (mapTRS, "mapTRS"),
    (mapTRSH, "mapTRSH"),
    (mapTRST, "mapTRST")
  ]

destImpls :: [(forall (r :: Type) a b. (Region r) => (a %1 -> b) -> [a] -> Dest r [b] %1 -> (), String)]
destImpls =
  [ (mapDestTRL, "mapDestTRL"),
    (mapDestTRS, "mapDestTRS"),
    (mapDestFL, "mapDestFL"),
    (mapDestFLS, "mapDestFLS"),
    (mapDestFSL, "mapDestFSL"),
    (mapDestFS, "mapDestFS")
  ]

safety :: forall a b. (Eq b, Show b) => [a] -> (a %1 -> b) -> TestTree
safety sampleData f =
  testGroup "safety" $
    ((tail nonDestImpls) <&> \(impl, implName) ->
      testCaseInfo ("mapL and " ++ implName ++ " give the same result") $ do
        let expected = mapL f sampleData
            actual = impl f sampleData
        assertEqual "same result" expected actual
        return $ show $ take (min 10 (length actual)) $ actual)
    ++
      (destImpls <&> \(impl, implName) ->
        testCaseInfo ("mapL and " ++ implName ++ " give the same result") $ do
          let expected = mapL f sampleData
              actual = unur (withRegion (\(_ :: Proxy r) t -> fromIncomplete_ (alloc @r t Lin.<&> \d -> impl f sampleData d)))
          assertEqual "same result" expected actual
          return $ show $ take (min 10 (length actual)) $ actual)

benchmark :: forall a b. (NFData b) => [a] -> (a %1 -> b) -> Benchmark
benchmark sampleData f =
  bgroup
    "benchmark"
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
