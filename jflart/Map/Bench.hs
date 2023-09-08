{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Map.Bench (benchmark, safety) where

import GHC.Compact (compact, getCompact)
import Test.Tasty.Bench
import Control.DeepSeq (force, NFData)
import Control.Exception (evaluate)
import Test.Tasty.HUnit
import Test.Tasty (testGroup, TestTree)
import Prelude.Linear (unur)
import Data.Functor ((<&>))

import Compact.Pure (RegionToken, withRegion)

import Map.Impl

safety :: forall a b. (Eq b, Show b) => [a] -> (a %1 -> b) -> TestTree
safety sampleData f =
  testGroup "safety" $ destImpls <&> \(impl, implName) ->
    testCaseInfo ("mapL and " ++ implName ++ " give the same result") $ do
      let ref = mapL f sampleData
          experimental = unur (withRegion (\(t :: RegionToken r) -> impl @r @a @b t f sampleData))
      assertEqual "same result" ref experimental
      return $ show $ take (min 10 (length sampleData)) $ experimental

--  :: (forall a' b'. (a' %1 -> b') -> [a'] %1 -> [b'])
--  :: (forall (r' :: Type) a' b'. RegionContext r' => RegionToken r' %1 -> (a' %1 -> b') -> [a'] -> Ur [b'])

benchmark :: forall a b. (NFData b) => [a] -> (a %1 -> b) -> Benchmark
benchmark sampleData f =
    bgroup
      "map implementations"
      (( concat $ nonDestImpls <&> \(impl, implName) ->
        [ bench (implName ++ " (with force)") $ (flip whnfAppIO) sampleData $ \sampleData -> do
              evaluate $ force $ impl @a @b f sampleData
        , bench (implName ++ " (with copy into region)") $ (flip whnfAppIO) sampleData $ \sampleData -> do
              resInReg <- compact $ impl @a @b f sampleData
              evaluate $ getCompact $ resInReg
        ]
      )
        ++ ( destImpls <&> \(impl, implName) ->
              bench implName $ (flip whnfAppIO) sampleData $ \sampleData -> do
                evaluate $ unur (withRegion (\(t :: RegionToken r) -> impl @r @a @b t f sampleData))
        )
      )