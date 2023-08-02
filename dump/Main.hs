{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}

module Main (main) where

import Compact.Pure
import Control.Functor.Linear ((<&>))
import Prelude.Linear
import GHC.Exts
import Unsafe.Coerce (unsafeCoerceAddr)

-- run with
-- cabal run -w /home/tbagrel/tweag/ghc/_build/stage1/bin/ghc --allow-newer --ghc-options='-threaded -O2 -rtsopts'

main :: IO ()
main = do
  let !list =
        case withRegion $ \r ->
          fromReg $ alloc r <&> \d -> go 0 1000 d of
            Ur l -> l
      go :: forall r. RegionContext r => Int -> Int -> Dest r [Int] %1 -> ()
      go i n d =
        if i < n
          then case d & fill @'(:) of
                 (dx, dl) -> (dx & fillLeaf i) `lseq` go (i + 1) n dl
          else d & fill @'[]
   in putStrLn (show $ head (tail list))
