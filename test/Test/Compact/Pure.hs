{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.Compact.Pure (compactPureTests) where

import Compact.Pure
import Control.Functor.Linear ((<&>))
import Control.Monad (return)
import GHC.Generics (Generic)
import Prelude.Linear hiding (Eq)
import Test.Tasty
import Test.Tasty.HUnit
import Prelude (Eq)

compactPureTests :: TestTree
compactPureTests =
  testGroup
    "With dests to fill compact region"
    [ testCaseInfo "Dests for compact region: compose when RHS is freshly allocated" compOnFreshAlloc,
      testCaseInfo "Dests for compact region: compose when RHS has already been filled" compOnUsedAlloc,
      testCaseInfo "Dests for compact region: fill custom data (via generic) and return companion value with fromRegionExtract" fillCustomDataAndExtract
    ]

-- Launch with

-- $ stack test
-- cabal test -w /home/thomas/tweag/ghc/_build/stage1/bin/ghc --allow-newer linear-dest:test:test --test-options='+RTS -N1 -RTS' --test-show-details=streaming

data Foo a b = MkFoo {unBar :: a, unBaz :: (b, b), unBoo :: a} deriving (Eq, Generic, Show)

compOnFreshAlloc :: IO String
compOnFreshAlloc = do
  let actual :: Ur (Int, Int)
      !actual = withRegion $ \(r :: RegionToken r) -> case dup2 r of
        (r', r'') ->
          fromRegion
            $ (alloc r')
            <&> ( \dp ->
                    case (dp & fill @'(,)) of
                      (dl, dr) ->
                        dl
                          & fillLeaf 1
                          `lseq` dr
                          & fillComp (alloc r'')
                          & fillLeaf 2
                )
      expected :: Ur (Int, Int)
      !expected = Ur (1, 2)
      fancyDisp = showHeap actual
  assertEqual "" expected actual
  return fancyDisp

compOnUsedAlloc :: IO String
compOnUsedAlloc = do
  let actual :: Ur (Int, (Int, Int))
      !actual = withRegion $ \r -> case dup2 r of
        (r', r'') ->
          fromRegion
            $ (alloc r')
            <&> ( \dp ->
                    case dp & fill @'(,) of
                      (dl, dr) ->
                        dl
                          & fillLeaf 1
                          `lseq` dr
                          & fillComp ((alloc r'') <&> (\dp' -> case dp' & fill @'(,) of (dr1, dr2) -> dr1 & fillLeaf 2 `lseq` dr2))
                          & fillLeaf 3
                )
      expected :: Ur (Int, (Int, Int))
      !expected = Ur (1, (2, 3))
      fancyDisp = showHeap actual
  assertEqual "" expected actual
  return fancyDisp

fillCustomDataAndExtract :: IO String
fillCustomDataAndExtract = do
  let actual :: Ur (Foo Int Char, Int)
      !actual = withRegion $ \r ->
        fromRegionExtract
          $ (alloc r)
          <&> ( \d ->
                  case d & fill @'MkFoo of
                    (dBar, dBaz, dBoo) ->
                      dBar
                        & fillLeaf 1
                        `lseq` ( case dBaz & fill @'(,) of
                                   (dl, dr) -> dl & fillLeaf 'a' `lseq` dr & fillLeaf 'b'
                               )
                        `lseq` dBoo
                        & fillLeaf 2
                        `lseq` Ur 14
              )
      expected :: Ur (Foo Int Char, Int)
      !expected = Ur (MkFoo 1 ('a', 'b') 2, 14)
      fancyDisp = showHeap actual
  assertEqual "" expected actual
  return fancyDisp
