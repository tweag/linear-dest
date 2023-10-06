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
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Map.Bench (benchmark, safety) where

import Compact.Pure (RegionToken, withRegion)
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Data.Functor ((<&>))
import GHC.Compact (compact, getCompact)
import Map.Impl
import Prelude.Linear (unur)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Bench
import Test.Tasty.HUnit

safety :: forall a b. (Eq b, Show b) => [a] -> (a %1 -> b) -> TestTree
safety sampleData f =
  testGroup "safety" $
    destImpls <&> \(impl, implName) ->
      testCaseInfo ("mapL and " ++ implName ++ " give the same result") $ do
        let ref = mapL f sampleData
            experimental = unur (withRegion (\(_ :: Proxy r) t -> impl @r @a @b t f sampleData))
        assertEqual "same result" ref experimental
        return $ show $ take (min 10 (length sampleData)) $ experimental

--  :: (forall a' b'. (a' %1 -> b') -> [a'] %1 -> [b'])
--  :: (forall (r' :: Type) a' b'. Region r' => Token' %1 -> (a' %1 -> b') -> [a'] -> Ur [b'])

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
                 evaluate $ unur (withRegion (\(_ :: Proxy r) t -> fromIncomplete_ $ alloc @r t <&> \dl -> impl @r @a @b f sampleData dl))
           )
    )
