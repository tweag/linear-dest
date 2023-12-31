{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK hide #-}

module Compact.Pure.Internal where

import Control.Functor.Linear (Data)
import Control.Functor.Linear qualified as Control
import Data.Data (Proxy (Proxy))
import Data.Functor.Linear qualified as Data
import Data.Kind (Type)
import Data.List (intercalate)
import Data.Reflection (Reifies (reflect), reify)
import Data.Replicator.Linear.Internal (Replicator (Moved))
import Data.Unrestricted.Linear.Internal.Consumable
import Data.Unrestricted.Linear.Internal.Dupable
import Data.Unrestricted.Linear.Internal.Ur
import GHC.Compact (Compact (..), compact, getCompact)
import GHC.Exts
import GHC.Generics
import GHC.IO (IO (..), unsafePerformIO)
import GHC.MVar (MVar (..))
import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerceAddr)
import Unsafe.Linear (toLinear, toLinear2)
import Prelude hiding (Monad, (>>=))

debugEnabled :: Bool
debugEnabled = False

ptrToWord :: Ptr a -> Word
ptrToWord (Ptr addr#) = W# (addr2Word# addr#)

addr2Word# :: Addr# -> Word#
addr2Word# addr# = int2Word# (addr2Int# addr#)

word2Addr# :: Word# -> Addr#
word2Addr# word# = int2Addr# (word2Int# word#)

alignAddr# :: Addr# -> Addr#
alignAddr# addr# =
  let word# = int2Word# (addr2Int# addr#)
      nmask# = int2Word# 7#
      wordAligned# = word# `and#` (not# nmask#)
   in int2Addr# (word2Int# wordAligned#)

indInfoPtr :: Ptr Word
indInfoPtr = Ptr (unsafeCoerceAddr (reifyStgInfoPtr# (# #) :: InfoPtrPlaceholder# "IND"))

showFill :: Ptr Word -> Ptr Word -> String -> [Ptr Word] -> String
showFill parentWriteLoc pX ctorName slots =
  "fill"
    ++ (show n)
    ++ ": @"
    ++ show (ptrToWord parentWriteLoc)
    ++ " <- #"
    ++ show (ptrToWord pX)
    ++ ": "
    ++ ctorName
    ++ " "
    ++ showSlots slots
  where
    n = length slots
    showSlots = intercalate " " . fmap showSlot
    showSlot ptr = "_@" ++ (show $ ptrToWord ptr)

putDebugLn :: String -> IO ()
putDebugLn x =
  if debugEnabled
    then putStrLn x
    else return ()
{-# INLINE putDebugLn #-}

putDebugLn# :: String -> (# State# RealWorld, res #) -> (# State# RealWorld, res #)
putDebugLn# text (# s0, res #) =
  if debugEnabled
    then case (putStrLn text) of IO f -> case f s0 of (# s1, () #) -> (# s1, res #)
    else (# s0, res #)
{-# INLINE putDebugLn# #-}

putInRegionIfNot# :: Compact# -> MVar# RealWorld () -> a -> State# RealWorld -> (# State# RealWorld, a #)
putInRegionIfNot# c# m# x = \s0 -> case compactContains# c# x s0 of
  (# s1, 1# #) -> (# s1, x #) -- already in region
  (# s1, _ #) -> case takeMVar# m# s1 of
    (# s2, () #) -> case compactAdd# c# x s2 of
      (# s3, xInRegion #) -> case putMVar# m# () s3 of
        s4 -> (# s4, xInRegion #)
{-# INLINE putInRegionIfNot# #-}

-- x must be already in the same region as the value, and fully evaluated
affect# :: Addr# -> a -> State# RealWorld -> (# State# RealWorld, Addr# #)
affect# dest xInRegion s0 = case anyToAddr# xInRegion s0 of
  (# s1, pXInRegion# #) -> case writeAddrOffAddr# (unsafeCoerceAddr dest) 0# pXInRegion# s1 of
    s2 -> (# s2, pXInRegion# #)
{-# INLINE affect# #-}

-------------------------------------------------------------------------------
-- RegionInfo and dests
-------------------------------------------------------------------------------

data FirstInhabitant = FirstInhabitant Int

firstInhabitant :: FirstInhabitant
firstInhabitant = FirstInhabitant 1234

newtype RegionInfo = RegionInfo (Compact FirstInhabitant)

data Token = Token

instance Consumable (Token) where
  consume Token = ()

instance Dupable (Token) where
  dupR Token = Moved Token

type Region r = Reifies r RegionInfo

getRegionInfo :: forall r. Region r => RegionInfo
getRegionInfo = reflect (Proxy :: Proxy r)
{-# INLINE getRegionInfo #-}

withRegion :: forall b. (forall (r :: Type). (Region r) => Proxy r -> Token %1 -> Ur b) %1 -> Ur b
withRegion = toLinear _withRegion
{-# INLINE withRegion #-}

{-# NOINLINE _withRegion #-}
_withRegion :: forall b. (forall (r :: Type). (Region r) => Proxy r -> Token %1 -> Ur b) -> Ur b
_withRegion f =
  unsafePerformIO $ do
    c <- (compact firstInhabitant)
    let !firstInhabitantInRegion = getCompact c
    firstPtr <- IO (\s -> case anyToAddr# firstInhabitantInRegion s of (# s', addr# #) -> (# s', W# (addr2Word# addr#) #))
    putDebugLn $
      "withRegion: allocating new region around @"
        ++ (show firstPtr)
    return $! reify (RegionInfo c) (\(proxy :: Proxy s) -> f @s proxy Token)

newtype Incomplete r a b = Incomplete (a, b)
  deriving (Data.Functor) via Data (Incomplete r a)
  deriving (Control.Functor)

piggyback :: forall r a b. (Region r) => Incomplete r a b %1 -> (Incomplete r a b, Token)
piggyback i = (i, Token)
{-# INLINE piggyback #-}

alloc :: forall r a. (Region r) => Token %1 -> Incomplete r a (Dest r a)
alloc = toLinear _alloc

{-# INLINE _alloc #-}
_alloc :: forall r a. (Region r) => Token -> Incomplete r a (Dest r a)
_alloc _ = case getRegionInfo @r of
  RegionInfo (Compact c# _ (MVar m#)) -> case indInfoPtr of
    Ptr indInfoAddr# -> case runRW# $ \s0 -> case takeMVar# m# s0 of
      (# s1, () #) -> case compactAddShallow# @a c# indInfoAddr# s1 of
        (# s2, indRoot #) -> case anyToAddr# indRoot s2 of
          (# s3, pIndRoot# #) -> case getSlots1# indRoot s3 of
            (# s4, (# dRoot# #) #) -> case putMVar# m# () s4 of
              s5 -> putDebugLn# message (# s5, Incomplete (indRoot, Dest dRoot#) #)
                where
                  message =
                    ( "alloc: [region] <- #"
                        ++ (show . ptrToWord $ Ptr pIndRoot#)
                        ++ ": IND _@"
                        ++ (show . ptrToWord $ Ptr dRoot#)
                    ) of
      (# _, res #) -> res

intoIncomplete :: forall r a. (Region r) => Token %1 -> a -> Incomplete r a ()
intoIncomplete = toLinear2 _intoIncomplete

{-# INLINE _intoIncomplete #-}
_intoIncomplete :: forall r a. (Region r) => Token -> a -> Incomplete r a ()
_intoIncomplete _ x = case getRegionInfo @r of
  RegionInfo (Compact c# _ (MVar m#)) -> case runRW# $ \s0 ->
    case putInRegionIfNot# c# m# x s0 of
      (# s1, xInRegion #) -> case anyToAddr# xInRegion s1 of
        (# s2, pXInRegion# #) -> putDebugLn# message (# s2, Incomplete (xInRegion, ()) #)
          where
            message =
              ( "intoIncomplete: [region] <- #"
                  ++ (show . ptrToWord $ Ptr pXInRegion#)
                  ++ ": [value]"
              ) of
    (# _, res #) -> res

fromIncomplete_ :: forall r a. (Region r) => Incomplete r a () %1 -> Ur a
fromIncomplete_ = toLinear _fromIncomplete_

{-# NOINLINE _fromIncomplete_ #-}
_fromIncomplete_ :: forall r a. (Region r) => Incomplete r a () -> Ur a
_fromIncomplete_ (Incomplete (root, ())) = case getRegionInfo @r of
  (RegionInfo (Compact c# _ (MVar m#))) -> case runRW# $ \s0 -> case takeMVar# m# s0 of
    (# s1, () #) -> case compactAddShallow# @(Ur a) c# ((unsafeCoerceAddr (reifyStgInfoPtr# (# #) :: InfoPtrPlaceholder# 'Ur))) s1 of
      (# s2, receiver #) -> case anyToAddr# receiver s2 of
        (# s3, pReceiver# #) -> case getSlots1# receiver s3 of
          (# s4, (# dRoot# #) #) -> case putMVar# m# () s4 of
            s5 -> case affect# dRoot# root s5 of
              (# s6, pRoot# #) -> putDebugLn# message (# s6, receiver #)
                where
                  message =
                    ( "fromIncomplete_: [region] <- #"
                        ++ (show . ptrToWord $ Ptr pReceiver#)
                        ++ ": Ur _@"
                        ++ (show . ptrToWord $ Ptr dRoot#)
                        ++ "\nfromIncomplete: @"
                        ++ (show . ptrToWord $ Ptr dRoot#)
                        ++ " <- #"
                        ++ (show . ptrToWord $ Ptr pRoot#)
                        ++ ": [root]"
                    ) of
    (# _, res #) -> res


fromIncomplete :: forall r a b. (Region r) => Incomplete r a (Ur b) %1 -> Ur (a, b)
fromIncomplete = toLinear _fromIncomplete

{-# NOINLINE _fromIncomplete #-}
_fromIncomplete :: forall r a b. (Region r) => Incomplete r a (Ur b) -> Ur (a, b)
_fromIncomplete (Incomplete (root, uCompanion)) = case getRegionInfo @r of
  (RegionInfo (Compact c# _ (MVar m#))) -> case runRW# $ \s0 -> case takeMVar# m# s0 of
    (# s1, () #) -> case compactAddShallow# @(Ur (a, b)) c# (unsafeCoerceAddr (reifyStgInfoPtr# (# #) :: InfoPtrPlaceholder# 'Ur)) s1 of
      (# s2, receiver #) -> case anyToAddr# receiver s2 of
        (# s3, pReceiver# #) -> case getSlots1# receiver s3 of
          (# s4, (# d# #) #) -> case compactAddShallow# @(a, b) c# (unsafeCoerceAddr (reifyStgInfoPtr# (# #) :: InfoPtrPlaceholder# '(,))) s4 of
            (# s5, pair #) -> case affect# d# pair s5 of
              (# s6, pPair# #) -> case getSlots2# pair s6 of
                (# s7, (# dRoot#, dCompanion# #) #) -> case putMVar# m# () s7 of
                  s8 -> case affect# dRoot# root s8 of
                    (# s9, pRoot# #) -> case uCompanion of
                      Ur companion -> case putInRegionIfNot# c# m# companion s9 of
                        (# s10, companionInRegion #) -> case affect# dCompanion# companionInRegion s10 of
                          (# s11, pCompanionInRegion# #) -> putDebugLn# message (# s11, receiver #)
                            where
                              message =
                                ( "fromIncomplete: [region] <- #"
                                    ++ (show . ptrToWord $ Ptr pReceiver#)
                                    ++ ": Ur _@"
                                    ++ (show . ptrToWord $ Ptr d#)
                                    ++ "\nfromIncomplete: @"
                                    ++ (show . ptrToWord $ Ptr d#)
                                    ++ " <- #"
                                    ++ (show . ptrToWord $ Ptr pPair#)
                                    ++ ": (,) _@"
                                    ++ (show . ptrToWord $ Ptr dRoot#)
                                    ++ " _@"
                                    ++ (show . ptrToWord $ Ptr dCompanion#)
                                    ++ "\nfromIncomplete: @"
                                    ++ (show . ptrToWord $ Ptr dRoot#)
                                    ++ " <- #"
                                    ++ (show . ptrToWord $ Ptr pRoot#)
                                    ++ ": [root]"
                                    ++ "\nfromIncomplete: @"
                                    ++ (show . ptrToWord $ Ptr dCompanion#)
                                    ++ " <- #"
                                    ++ (show . ptrToWord $ Ptr pCompanionInRegion#)
                                    ++ ": [companion]"
                                ) of
    (# _, res #) -> res

-------------------------------------------------------------------------------
-- Metaprogramming stuff for dests
-------------------------------------------------------------------------------

data Dest r a = Dest Addr#

fill :: forall lCtor (r :: Type) (a :: Type). (Fill lCtor a, Region r) => Dest r a %1 -> DestsOf lCtor r a
fill = toLinear (_fill @lCtor @a @r)
{-# INLINE fill #-}

fillComp :: forall r a b. (Region r) => Incomplete r a b %1 -> Dest r a %1 -> b
fillComp = toLinear2 _fillComp
{-# INLINE fillComp #-}

fillLeaf :: forall r a. (Region r) => a -> Dest r a %1 -> ()
fillLeaf x = fillComp (intoIncomplete @r Token x)
{-# INLINE fillLeaf #-}

_fillComp :: forall r a b. (Region r) => Incomplete r a b -> Dest r a -> b
_fillComp (Incomplete (root, companion)) (Dest d#) = case runRW# $ \s0 -> case affect# d# root s0 of
  (# s1, pRoot# #) -> putDebugLn# message (# s1, companion #)
    where
      message =
        ( "fillComp: @"
            ++ (show . ptrToWord $ Ptr d#)
            ++ " <- #"
            ++ (show . ptrToWord $ Ptr pRoot#)
            ++ ": [Incomplete OR value]"
        ) of
  (# _, res #) -> res

class Fill lCtor (a :: Type) where
  _fill :: forall (r :: Type). Region r => Dest r a -> DestsOf lCtor r a

instance (specCtor ~ LiftedCtorToSpecCtor lCtor a, GFill# lCtor specCtor a) => Fill lCtor a where
  _fill :: forall (r :: Type). Region r =>  Dest r a -> DestsOf lCtor r a
  _fill (Dest d#) = case getRegionInfo @r of
    (RegionInfo (Compact c# _ (MVar m#))) -> case runRW# (gFill# @lCtor @specCtor @a @r c# m# d#) of (# _, res #) -> res
  {-# INLINE _fill #-}

-- ctor :: (Meta, [(Meta, Type)])
class GFill# lCtor (specCtor :: (Meta, [(Meta, Type)])) (a :: Type) where
  gFill# :: forall (r :: Type). Compact# -> MVar# RealWorld () -> Addr# -> State# RealWorld -> (# State# RealWorld, GDestsOf specCtor r #)

instance (Generic a, repA ~ Rep a (), metaA ~ GDatatypeMetaOf repA, Datatype metaA, 'MetaCons symCtor fix hasSel ~ metaCtor, Constructor metaCtor, LiftedCtorToSymbol lCtor ~ symCtor, 'Just '(metaCtor, '[]) ~ GSpecCtorOf symCtor (Rep a ())) => GFill# lCtor '(metaCtor, '[]) a where
  gFill# :: forall (r :: Type). Compact# -> MVar# RealWorld () -> Addr# -> State# RealWorld -> (# State# RealWorld, GDestsOf '(metaCtor, '[]) r #)
  gFill# c# m# d# s0 =
    case takeMVar# m# s0 of
      (# s1, () #) ->
        case compactAddShallow# c# (unsafeCoerceAddr (reifyStgInfoPtr# (# #) :: InfoPtrPlaceholder# lCtor)) s1 of
          (# s2, xInRegion #) -> case affect# d# xInRegion s2 of
            (# s3, pXInRegion# #) -> case putMVar# m# () s3 of
              s4 -> putDebugLn# (showFill (Ptr d#) (Ptr pXInRegion#) (conName @metaCtor undefined) []) (# s4, () #)
  {-# INLINE gFill# #-}

-- TODO: add constraints on ds_i variables to ensure no unpacking
instance (Generic a, repA ~ Rep a (), metaA ~ GDatatypeMetaOf repA, Datatype metaA, 'MetaCons symCtor fix hasSel ~ metaCtor, Constructor metaCtor, LiftedCtorToSymbol lCtor ~ symCtor, 'Just '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0)]) ~ GSpecCtorOf symCtor (Rep a ())) => GFill# lCtor '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0)]) a where
  gFill# :: forall (r :: Type). Compact# -> MVar# RealWorld () -> Addr# -> State# RealWorld -> (# State# RealWorld, GDestsOf '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0)]) r #)
  gFill# c# m# d# s0 =
    case takeMVar# m# s0 of
      (# s1, () #) ->
        case compactAddShallow# c# (unsafeCoerceAddr (reifyStgInfoPtr# (# #) :: InfoPtrPlaceholder# lCtor)) s1 of
          (# s2, xInRegion #) -> case affect# d# xInRegion s2 of
            (# s3, pXInRegion# #) -> case getSlots1# xInRegion s3 of
              (# s4, (# d0# #) #) -> case putMVar# m# () s4 of
                s5 -> putDebugLn# (showFill (Ptr d#) (Ptr pXInRegion#) (conName @metaCtor undefined) [Ptr d0#]) (# s5, (Dest d0# :: Dest r t0) #)
  {-# INLINE gFill# #-}

-- TODO: add constraints on ds_i variables to ensure no unpacking
instance (Generic a, repA ~ Rep a (), metaA ~ GDatatypeMetaOf repA, Datatype metaA, 'MetaCons symCtor fix hasSel ~ metaCtor, Constructor metaCtor, LiftedCtorToSymbol lCtor ~ symCtor, 'Just '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1)]) ~ GSpecCtorOf symCtor (Rep a ())) => GFill# lCtor '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1)]) a where
  gFill# :: forall (r :: Type). Compact# -> MVar# RealWorld () -> Addr# -> State# RealWorld -> (# State# RealWorld, GDestsOf '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1)]) r #)
  gFill# c# m# d# s0 =
    case takeMVar# m# s0 of
      (# s1, () #) ->
        case compactAddShallow# c# (unsafeCoerceAddr (reifyStgInfoPtr# (# #) :: InfoPtrPlaceholder# lCtor)) s1 of
          (# s2, xInRegion #) -> case affect# d# xInRegion s2 of
            (# s3, pXInRegion# #) -> case getSlots2# xInRegion s3 of
              (# s4, (# d0#, d1# #) #) -> case putMVar# m# () s4 of
                s5 -> putDebugLn# (showFill (Ptr d#) (Ptr pXInRegion#) (conName @metaCtor undefined) [Ptr d0#, Ptr d1#]) (# s5, (Dest d0# :: Dest r t0, Dest d1# :: Dest r t1) #)
  {-# INLINE gFill# #-}

-- TODO: add constraints on ds_i variables to ensure no unpacking
instance (Generic a, repA ~ Rep a (), metaA ~ GDatatypeMetaOf repA, Datatype metaA, 'MetaCons symCtor fix hasSel ~ metaCtor, Constructor metaCtor, LiftedCtorToSymbol lCtor ~ symCtor, 'Just '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1), '( 'MetaSel f2 u2 ss2 ds2, t2)]) ~ GSpecCtorOf symCtor (Rep a ())) => GFill# lCtor '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1), '( 'MetaSel f2 u2 ss2 ds2, t2)]) a where
  gFill# :: forall (r :: Type). Compact# -> MVar# RealWorld () -> Addr# -> State# RealWorld -> (# State# RealWorld, GDestsOf '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1), '( 'MetaSel f2 u2 ss2 ds2, t2)]) r #)
  gFill# c# m# d# s0 =
    case takeMVar# m# s0 of
      (# s1, () #) ->
        case compactAddShallow# c# (unsafeCoerceAddr (reifyStgInfoPtr# (# #) :: InfoPtrPlaceholder# lCtor)) s1 of
          (# s2, xInRegion #) -> case affect# d# xInRegion s2 of
            (# s3, pXInRegion# #) -> case getSlots3# xInRegion s3 of
              (# s4, (# d0#, d1#, d2# #) #) -> case putMVar# m# () s4 of
                s5 -> putDebugLn# (showFill (Ptr d#) (Ptr pXInRegion#) (conName @metaCtor undefined) [Ptr d0#, Ptr d1#, Ptr d2#]) (# s5, (Dest d0# :: Dest r t0, Dest d1# :: Dest r t1, Dest d2# :: Dest r t2) #)
  {-# INLINE gFill# #-}

-- TODO: add constraints on ds_i variables to ensure no unpacking
instance (Generic a, repA ~ Rep a (), metaA ~ GDatatypeMetaOf repA, Datatype metaA, 'MetaCons symCtor fix hasSel ~ metaCtor, Constructor metaCtor, LiftedCtorToSymbol lCtor ~ symCtor, 'Just '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1), '( 'MetaSel f2 u2 ss2 ds2, t2), '( 'MetaSel f3 u3 ss3 ds3, t3)]) ~ GSpecCtorOf symCtor (Rep a ())) => GFill# lCtor '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1), '( 'MetaSel f2 u2 ss2 ds2, t2), '( 'MetaSel f3 u3 ss3 ds3, t3)]) a where
  gFill# :: forall (r :: Type). Compact# -> MVar# RealWorld () -> Addr# -> State# RealWorld -> (# State# RealWorld, GDestsOf '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1), '( 'MetaSel f2 u2 ss2 ds2, t2), '( 'MetaSel f3 u3 ss3 ds3, t3)]) r #)
  gFill# c# m# d# s0 =
    case takeMVar# m# s0 of
      (# s1, () #) ->
        case compactAddShallow# c# (unsafeCoerceAddr (reifyStgInfoPtr# (# #) :: InfoPtrPlaceholder# lCtor)) s1 of
          (# s2, xInRegion #) -> case affect# d# xInRegion s2 of
            (# s3, pXInRegion# #) -> case getSlots4# xInRegion s3 of
              (# s4, (# d0#, d1#, d2#, d3# #) #) -> case putMVar# m# () s4 of
                s5 -> putDebugLn# (showFill (Ptr d#) (Ptr pXInRegion#) (conName @metaCtor undefined) [Ptr d0#, Ptr d1#, Ptr d2#, Ptr d3#]) (# s5, (Dest d0# :: Dest r t0, Dest d1# :: Dest r t1, Dest d2# :: Dest r t2, Dest d3# :: Dest r t3) #)
  {-# INLINE gFill# #-}

-- TODO: add constraints on ds_i variables to ensure no unpacking
instance (Generic a, repA ~ Rep a (), metaA ~ GDatatypeMetaOf repA, Datatype metaA, 'MetaCons symCtor fix hasSel ~ metaCtor, Constructor metaCtor, LiftedCtorToSymbol lCtor ~ symCtor, 'Just '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1), '( 'MetaSel f2 u2 ss2 ds2, t2), '( 'MetaSel f3 u3 ss3 ds3, t3), '( 'MetaSel f4 u4 ss4 ds4, t4)]) ~ GSpecCtorOf symCtor (Rep a ())) => GFill# lCtor '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1), '( 'MetaSel f2 u2 ss2 ds2, t2), '( 'MetaSel f3 u3 ss3 ds3, t3), '( 'MetaSel f4 u4 ss4 ds4, t4)]) a where
  gFill# :: forall (r :: Type). Compact# -> MVar# RealWorld () -> Addr# -> State# RealWorld -> (# State# RealWorld, GDestsOf '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1), '( 'MetaSel f2 u2 ss2 ds2, t2), '( 'MetaSel f3 u3 ss3 ds3, t3), '( 'MetaSel f4 u4 ss4 ds4, t4)]) r #)
  gFill# c# m# d# s0 =
    case takeMVar# m# s0 of
      (# s1, () #) ->
        case compactAddShallow# c# (unsafeCoerceAddr (reifyStgInfoPtr# (# #) :: InfoPtrPlaceholder# lCtor)) s1 of
          (# s2, xInRegion #) -> case affect# d# xInRegion s2 of
            (# s3, pXInRegion# #) -> case getSlots5# xInRegion s3 of
              (# s4, (# d0#, d1#, d2#, d3#, d4# #) #) -> case putMVar# m# () s4 of
                s5 -> putDebugLn# (showFill (Ptr d#) (Ptr pXInRegion#) (conName @metaCtor undefined) [Ptr d0#, Ptr d1#, Ptr d2#, Ptr d3#, Ptr d4#]) (# s5, (Dest d0# :: Dest r t0, Dest d1# :: Dest r t1, Dest d2# :: Dest r t2, Dest d3# :: Dest r t3, Dest d4# :: Dest r t4) #)
  {-# INLINE gFill# #-}

-- TODO: add constraints on ds_i variables to ensure no unpacking
instance (Generic a, repA ~ Rep a (), metaA ~ GDatatypeMetaOf repA, Datatype metaA, 'MetaCons symCtor fix hasSel ~ metaCtor, Constructor metaCtor, LiftedCtorToSymbol lCtor ~ symCtor, 'Just '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1), '( 'MetaSel f2 u2 ss2 ds2, t2), '( 'MetaSel f3 u3 ss3 ds3, t3), '( 'MetaSel f4 u4 ss4 ds4, t4), '( 'MetaSel f5 u5 ss5 ds5, t5)]) ~ GSpecCtorOf symCtor (Rep a ())) => GFill# lCtor '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1), '( 'MetaSel f2 u2 ss2 ds2, t2), '( 'MetaSel f3 u3 ss3 ds3, t3), '( 'MetaSel f4 u4 ss4 ds4, t4), '( 'MetaSel f5 u5 ss5 ds5, t5)]) a where
  gFill# :: forall (r :: Type). Compact# -> MVar# RealWorld () -> Addr# -> State# RealWorld -> (# State# RealWorld, GDestsOf '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1), '( 'MetaSel f2 u2 ss2 ds2, t2), '( 'MetaSel f3 u3 ss3 ds3, t3), '( 'MetaSel f4 u4 ss4 ds4, t4), '( 'MetaSel f5 u5 ss5 ds5, t5)]) r #)
  gFill# c# m# d# s0 =
    case takeMVar# m# s0 of
      (# s1, () #) ->
        case compactAddShallow# c# (unsafeCoerceAddr (reifyStgInfoPtr# (# #) :: InfoPtrPlaceholder# lCtor)) s1 of
          (# s2, xInRegion #) -> case affect# d# xInRegion s2 of
            (# s3, pXInRegion# #) -> case getSlots6# xInRegion s3 of
              (# s4, (# d0#, d1#, d2#, d3#, d4#, d5# #) #) -> case putMVar# m# () s4 of
                s5 -> putDebugLn# (showFill (Ptr d#) (Ptr pXInRegion#) (conName @metaCtor undefined) [Ptr d0#, Ptr d1#, Ptr d2#, Ptr d3#, Ptr d4#, Ptr d5#]) (# s5, (Dest d0# :: Dest r t0, Dest d1# :: Dest r t1, Dest d2# :: Dest r t2, Dest d3# :: Dest r t3, Dest d4# :: Dest r t4, Dest d5# :: Dest r t5) #)
  {-# INLINE gFill# #-}

-- TODO: add constraints on ds_i variables to ensure no unpacking
instance (Generic a, repA ~ Rep a (), metaA ~ GDatatypeMetaOf repA, Datatype metaA, 'MetaCons symCtor fix hasSel ~ metaCtor, Constructor metaCtor, LiftedCtorToSymbol lCtor ~ symCtor, 'Just '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1), '( 'MetaSel f2 u2 ss2 ds2, t2), '( 'MetaSel f3 u3 ss3 ds3, t3), '( 'MetaSel f4 u4 ss4 ds4, t4), '( 'MetaSel f5 u5 ss5 ds5, t5), '( 'MetaSel f6 u6 ss6 ds6, t6)]) ~ GSpecCtorOf symCtor (Rep a ())) => GFill# lCtor '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1), '( 'MetaSel f2 u2 ss2 ds2, t2), '( 'MetaSel f3 u3 ss3 ds3, t3), '( 'MetaSel f4 u4 ss4 ds4, t4), '( 'MetaSel f5 u5 ss5 ds5, t5), '( 'MetaSel f6 u6 ss6 ds6, t6)]) a where
  gFill# :: forall (r :: Type). Compact# -> MVar# RealWorld () -> Addr# -> State# RealWorld -> (# State# RealWorld, GDestsOf '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1), '( 'MetaSel f2 u2 ss2 ds2, t2), '( 'MetaSel f3 u3 ss3 ds3, t3), '( 'MetaSel f4 u4 ss4 ds4, t4), '( 'MetaSel f5 u5 ss5 ds5, t5), '( 'MetaSel f6 u6 ss6 ds6, t6)]) r #)
  gFill# c# m# d# s0 =
    case takeMVar# m# s0 of
      (# s1, () #) ->
        case compactAddShallow# c# (unsafeCoerceAddr (reifyStgInfoPtr# (# #) :: InfoPtrPlaceholder# lCtor)) s1 of
          (# s2, xInRegion #) -> case affect# d# xInRegion s2 of
            (# s3, pXInRegion# #) -> case getSlots7# xInRegion s3 of
              (# s4, (# d0#, d1#, d2#, d3#, d4#, d5#, d6# #) #) -> case putMVar# m# () s4 of
                s5 -> putDebugLn# (showFill (Ptr d#) (Ptr pXInRegion#) (conName @metaCtor undefined) [Ptr d0#, Ptr d1#, Ptr d2#, Ptr d3#, Ptr d4#, Ptr d5#, Ptr d6#]) (# s5, (Dest d0# :: Dest r t0, Dest d1# :: Dest r t1, Dest d2# :: Dest r t2, Dest d3# :: Dest r t3, Dest d4# :: Dest r t4, Dest d5# :: Dest r t5, Dest d6# :: Dest r t6) #)
  {-# INLINE gFill# #-}

type family GDestsOf (specCtor :: (Meta, [(Meta, Type)])) (r :: Type) :: Type where
  GDestsOf '(_, '[]) _ = ()
  GDestsOf '(_, '[ '(_, t)]) r = Dest r t
  GDestsOf '(_, '[ '(_, t0), '(_, t1)]) r = (Dest r t0, Dest r t1)
  GDestsOf '(_, '[ '(_, t0), '(_, t1), '(_, t2)]) r = (Dest r t0, Dest r t1, Dest r t2)
  GDestsOf '(_, '[ '(_, t0), '(_, t1), '(_, t2), '(_, t3)]) r = (Dest r t0, Dest r t1, Dest r t2, Dest r t3)
  GDestsOf '(_, '[ '(_, t0), '(_, t1), '(_, t2), '(_, t3), '(_, t4)]) r = (Dest r t0, Dest r t1, Dest r t2, Dest r t3, Dest r t4)
  GDestsOf '(_, '[ '(_, t0), '(_, t1), '(_, t2), '(_, t3), '(_, t4), '(_, t5)]) r = (Dest r t0, Dest r t1, Dest r t2, Dest r t3, Dest r t4, Dest r t5)
  GDestsOf '(_, '[ '(_, t0), '(_, t1), '(_, t2), '(_, t3), '(_, t4), '(_, t5), '(_, t6)]) r = (Dest r t0, Dest r t1, Dest r t2, Dest r t3, Dest r t4, Dest r t5, Dest r t6)
  GDestsOf _ _ = TypeError ('Text "GDestsOf not implemented for constructors with more than 7 fields")

type family DestsOf lCtor (r :: Type) (a :: Type) where
  DestsOf lCtor r a = GDestsOf (LiftedCtorToSpecCtor lCtor a) r

type family GDatatypeMetaOf (repA :: Type) :: Meta where
  GDatatypeMetaOf (D1 meta f p) = meta
  GDatatypeMetaOf (M1 _ _ f p) = GDatatypeMetaOf (f p)
  GDatatypeMetaOf _ = TypeError ('Text "No match for GDatatypeMetaOf")

type family GFieldsOf (repA :: Type) :: [(Meta, Type)] where
  GFieldsOf (S1 meta f p) = '[ '(meta, GSlotTypeOf (f p))]
  GFieldsOf (U1 _) = '[]
  GFieldsOf ((f :*: g) p) = GFieldsOf (f p) ++ GFieldsOf (g p)
  GFieldsOf (M1 _ _ f p) = GFieldsOf (f p)
  GFieldsOf _ = TypeError ('Text "No match for GFieldsOf")

type family GSlotTypeOf (repA :: Type) :: Type where
  GSlotTypeOf (K1 _ c _) = c
  GSlotTypeOf (M1 _ _ f p) = GSlotTypeOf (f p)
  GSlotTypeOf _ = TypeError ('Text "No match for GSlotTypeOf")

type family GSpecCtorOf (symCtor :: Symbol) (repA :: Type) :: Maybe (Meta, [(Meta, Type)]) where
  GSpecCtorOf symCtor (C1 ('MetaCons symCtor x y) f p) = 'Just '(('MetaCons symCtor x y), GFieldsOf (f p))
  GSpecCtorOf symCtor (C1 ('MetaCons _ _ _) _ _) = 'Nothing
  GSpecCtorOf symCtor ((f :+: g) p) = GSpecCtorOf symCtor (f p) <|> GSpecCtorOf symCtor (g p)
  GSpecCtorOf symCtor (V1 _) = 'Nothing
  GSpecCtorOf symCtor (M1 _ _ f p) = GSpecCtorOf symCtor (f p)
  GSpecCtorOf _ _ = TypeError ('Text "No match for GHasCtor")

type family LiftedCtorToSpecCtor lCtor (a :: Type) :: (Meta, [(Meta, Type)]) where
  LiftedCtorToSpecCtor lCtor a = FromJust (GSpecCtorOf (LiftedCtorToSymbol lCtor) (Rep a ()))

type family (a :: [k]) ++ (b :: [k]) :: [k] where
  '[] ++ y = y
  (x : xs) ++ y = x : (xs ++ y)
  _ ++ _ = TypeError ('Text "No match for ++")

type family FromJust (x :: Maybe k) :: k where
  FromJust ('Just v) = v
  FromJust 'Nothing = TypeError ('Text "FromJust error: got 'Nothing")

type family (x :: Maybe k) <|> (y :: Maybe k) :: Maybe k where
  ('Just v) <|> _ = 'Just v
  'Nothing <|> y = y