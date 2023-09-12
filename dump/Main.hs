{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}

-- {-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}

module Main (main) where

import Compact.Pure
import Compact.Pure.SExpr
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Functor.Linear ((<&>))
import GHC.Compact (compact, getCompact)
import GHC.Exts
import System.Environment
import Unsafe.Coerce (unsafeCoerceAddr)

-- run with
-- cabal run -w /home/thomas/tweag/ghc/_build/stage1/bin/ghc --allow-newer --ghc-options='-threaded -O2 -rtsopts' exe:dump -- +RTS -s -RTS runParseWithoutDestForce
-- cabal run -w /home/thomas/tweag/ghc/_build/stage1/bin/ghc --allow-newer --ghc-options='-threaded -O2 -rtsopts' exe:dump -- +RTS -s -RTS runParseWithoutDestCopyReg
-- cabal run -w /home/thomas/tweag/ghc/_build/stage1/bin/ghc --allow-newer --ghc-options='-threaded -O2 -rtsopts' exe:dump -- +RTS -s -RTS runParseWithDest

main :: IO ()
main = do
  args <- getArgs
  !sampleData <- evaluate =<< force <$> loadSampleData
  res <- case args of
    "runParseWithoutDestForce" : _ -> do
      let res = parseWithoutDest sampleData
      evaluate . force $ res
    "runParseWithoutDestCopyReg" : _ -> do
      let res = parseWithoutDest sampleData
      compResInRegion <- compact res
      evaluate . getCompact $ compResInRegion
    "runParseWithDest" : _ -> do
      evaluate . parseWithDest $ sampleData
  putStrLn $ case res of
    Right _ -> "Done!"
    _ -> "Error!"

-- -- ### For blogpost ###
-- import Foreign.Storable
-- import GHC.IO (IO (..))

-- wordToPtrW :: Word -> Ptr Word
-- wordToPtrW (W# w#) = Ptr (int2Addr# (word2Int# w#))

-- alignAddr# :: Addr# -> Addr#
-- alignAddr# addr# =
--   let word# = int2Word# (addr2Int# addr#)
--       nmask# = int2Word# 7#
--       wordAligned# = word# `and#` (not# nmask#)
--    in int2Addr# (word2Int# wordAligned#)

-- naiveInspectInfoPtr :: String -> a -> IO ()
-- naiveInspectInfoPtr prefix x = do
--   xPtr <- (IO $ \s0 -> case anyToAddr# x s0 of (# s1, addr# #) -> (# s1, (Ptr (alignAddr# addr#) :: Ptr Word) #))
--   xInfoTablePtr <- peek xPtr
--   putStrLn $ prefix ++ show (wordToPtrW xInfoTablePtr)

-- coerceToPtr :: forall k (a :: k). InfoPtrPlaceholder# a -> Ptr Word
-- coerceToPtr addrLike# = Ptr (unsafeCoerceAddr addrLike#)

-- main :: IO ()
-- main = do
--     let !x = Just (1 :: Int)
--     naiveInspectInfoPtr "naive inspection of Just infoTblPtr: " x
--     putStrLn $ "reified Just infoTblPtr: " ++ (show $ coerceToPtr (reifyStgInfoPtr# (# #) :: InfoPtrPlaceholder# 'Just))
--     putStrLn $ "reified BLACKHOLE infoTblPtr: " ++ (show $ coerceToPtr (reifyStgInfoPtr# (# #) :: InfoPtrPlaceholder# "BLACKHOLE"))
