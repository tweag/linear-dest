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

module DList.Bench (benchmark, safety, getBenchgroup) where

import Compact.Pure
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Data.Functor ((<&>))
import DList.Impl
import Prelude.Linear (unur, dup2, consume, lseq)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Bench
import Test.Tasty.HUnit
import Data.Proxy (Proxy)
import Data.List.Linear ((++))
import Prelude hiding (foldr, concat, (++))

impls :: [(forall a. [[a]] -> [a], String, Bool)]
impls =
  [ (concatLeft, "concatLeft", True)
  , (concatRight, "concatRight", True)
  , (differenceListNaiveLeft, "differenceListNaiveLeft", True)
  , (differenceListDestLeft, "differenceListDestLeft", False)
  ]

foldl' :: forall a b. (a %1 -> b -> a) -> a %1 -> [b] -> a
foldl' _ s [] = s
foldl' f s (x : xs) = let !r = (f s x) in foldl' f r xs

foldr :: forall a b. (b -> a %1 -> a) -> a %1 -> [b] -> a
foldr _ s [] = s
foldr f s (x : xs) = x `f` foldr f s xs

loadListsData :: IO [[Int]]
loadListsData = evaluate =<< force <$> return ([0..999] <&> (\i -> [(100 * i + 0)..(100 * i + 99)]))

concatLeft :: [[a]] -> [a]
concatLeft = foldl' (\xs ys -> xs ++ ys) []

concatRight :: [[a]] -> [a]
concatRight = foldr (\xs ys -> xs ++ ys) []

differenceListNaiveLeft :: [[a]] -> [a]
differenceListNaiveLeft lists = toListN (foldl' (\dl ys -> let !r = dl `concatN` (fromListN ys) in r) newN lists)

differenceListDestLeft :: [[a]] -> [a]
differenceListDestLeft lists = unur (withRegion (\(_ :: Proxy r) t ->
  let f :: (Token, DList r a) %1 -> [a] -> (Token, DList r a)
      f (t, dl) ys =
        let !(t', t'') = dup2 t
            !r = dl `concat` (fromList @r t' ys)
         in (t'', r)
      !(t', t'') = dup2 t
      !(t''', dl) = foldl' f (t'', new @r t') lists
   in consume t''' `lseq` toUList dl ))

safety :: forall a. (Eq a, Show a) => [[a]] -> TestTree
safety sampleData =
  testGroup "safety" $
     (tail impls) <&> \(impl, implName, _) ->
      testCaseInfo ("concatLeft and " ++ implName ++ " give the same result") $ do
        let ref = concatLeft sampleData
            experimental = impl sampleData
        assertEqual "same result" ref experimental
        return $ show $ take (min 102 (length experimental)) $ experimental

benchmark :: forall a. NFData a => [[a]] -> Benchmark
benchmark sampleData =
  bgroup "difference list implementations" $
    impls <&> \(impl, implName, isLazy) -> bench implName $ (flip whnfAppIO) sampleData $ \sampleData -> do
      evaluate $ (if isLazy then force else id) $ impl sampleData

getBenchgroup :: IO Benchmark
getBenchgroup = do
  !listsData <- loadListsData
  return $ bgroup "difference list"
    [ benchmark listsData,
      safety listsData
    ]
