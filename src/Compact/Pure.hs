{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Compact.Pure
  ( ShowHeap (_showHeap),
    _showHeapPrim,
    showHeap,
    RegionContext,
    RegionToken,
    withRegion,
    withRegionM,
    getToken,
    infuseToken,
    Dest,
    Fill (fill),
    DestsOf,
    fillComp,
    fillLeaf,
    intoReg,
    fromReg,
    fromRegM,
    fromRegExtract,
    Incomplete,
    alloc,
    indInfoPtr,
  )
where

import Compact.Pure.Internal
