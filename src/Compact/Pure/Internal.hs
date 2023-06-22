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
import Control.Monad (forM)
import Data.Data (Proxy (Proxy))
import Data.Functor.Linear qualified as Data
import Data.Kind (Type)
import Data.List (intercalate)
import Data.Reflection (Reifies (reflect), reify)
import Data.Replicator.Linear.Internal (Replicator (Moved))
import Data.Semigroup (stimesMonoid)
import Data.Unrestricted.Linear.Internal.Consumable
import Data.Unrestricted.Linear.Internal.Dupable
import Data.Unrestricted.Linear.Internal.Ur
import Foreign (peek, plusPtr)
import GHC.Compact (Compact (..), compact, getCompact)
import GHC.Exts
import GHC.Generics
import GHC.IO (IO (..), unsafeDupablePerformIO, unsafePerformIO)
import GHC.MVar (MVar (..))
import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce, unsafeCoerceAddr)
import Unsafe.Linear (toLinear, toLinear2)

-------------------------------------------------------------------------------
-- Helpers for display/debug
-------------------------------------------------------------------------------

isProfilingEnabled :: Bool
isProfilingEnabled = unsafePerformIO $ do
  intInReg <- getCompact <$> compact (1 :: Word)
  let intAddr = aToRawPtr intInReg
  v <- peek $ intAddr `plusPtr` wordSize
  return $ v /= (1 :: Word)

wordSize :: Int
wordSize = 8

headerSize :: Int
headerSize = if isProfilingEnabled then 24 else 8

debugEnabled :: Bool
debugEnabled = False

placeholder :: Int
placeholder = 1339

putDebugLn# :: String -> (# State# RealWorld, res #) -> (# State# RealWorld, res #)
putDebugLn# text (# s0, res #) =
  if debugEnabled
    then case (putStrLn text) of IO f -> case f s0 of (# s1, () #) -> (# s1, res #)
    else (# s0, res #)
{-# INLINE putDebugLn# #-}

putDebugLn :: String -> IO ()
putDebugLn x =
  if debugEnabled
    then putStrLn x
    else return ()
{-# INLINE putDebugLn #-}

-------------------------------------------------------------------------------
-- Primitives to do unsafe things
-------------------------------------------------------------------------------

data Ptr' a = Ptr' a

ptrToPtr' :: Ptr a -> Ptr' a
ptrToPtr' p = let !r = p in unsafeCoerce r

ptr'ToPtr :: Ptr' a -> Ptr a
ptr'ToPtr p = let !r = p in unsafeCoerce r

instance (Show a) => Show (Ptr' a) where
  show (Ptr' x) = "Ptr' " ++ show x

{-# INLINE align# #-}
align# :: Int# -> Word# -> Word#
align# wordSize# w# =
  let mask = int2Word# (wordSize# -# 1#)
   in w# `and#` (not# mask)

-------------------------------------------------------------------------------
-- Helpers to do unsafe things derived from primitives above
-------------------------------------------------------------------------------

addr2Word# :: Addr# -> Word#
addr2Word# addr# = int2Word# (addr2Int# addr#)

word2Addr# :: Word# -> Addr#
word2Addr# word# = int2Addr# (word2Int# word#)

align :: Ptr a -> Ptr Word
align (Ptr addr#) = do
  let !(I# wordSize#) = wordSize
      word# = addr2Word# addr#
      wordAligned# = align# wordSize# word#
      addrAligned# = word2Addr# wordAligned#
   in Ptr addrAligned#

ptrToA :: Ptr a -> a
ptrToA p =
  case ptrToPtr' p of
    Ptr' res -> res

aToPtr :: a -> Ptr a
aToPtr x = ptr'ToPtr (Ptr' x)

aToRawPtr :: a -> Ptr Word
aToRawPtr x = align (aToPtr x)

aToWord :: a -> Word
aToWord x = ptrToWord (aToPtr x)

wordToA :: Word -> a
wordToA w = ptrToA (wordToPtr w)

ptrToWord :: Ptr a -> Word
ptrToWord (Ptr addr#) = W# (addr2Word# addr#)

wordToPtr :: Word -> Ptr a
wordToPtr (W# word#) = Ptr (word2Addr# word#)

intToWord :: Int -> Word
intToWord (I# int#) = W# (int2Word# int#)

wordToInt :: Word -> Int
wordToInt (W# word#) = I# (word2Int# word#)

wordToPtr' :: Word -> Ptr' a
wordToPtr' w = ptrToPtr' (wordToPtr w)

ptr'ToWord :: Ptr' a -> Word
ptr'ToWord p = ptrToWord (ptr'ToPtr p)

peekInfoPtr :: a -> IO Word
peekInfoPtr x = do
  let rawPtr = aToRawPtr x
  peek rawPtr

getCtorInfoPtrFromSym :: forall (symCtor :: Symbol) (a :: Type). (Generic a, GShallow symCtor (Rep a ())) => IO Word
getCtorInfoPtrFromSym = let !evaluated = shallowTerm @symCtor @a in peekInfoPtr evaluated

showRaw :: Int -> a -> IO String
showRaw n x =
  unwords <$> do
    let p = aToRawPtr x
    h <- forM [0 .. (headerSize `div` wordSize) - 1] $ \k -> do
      w <- peek (p `plusPtr` (k * wordSize)) :: IO Word
      return $ "[" ++ show k ++ "]" ++ show w
    r <- forM [0 .. n - 1] $ \k -> do
      w <- peek (p `plusPtr` headerSize `plusPtr` (k * wordSize)) :: IO Word
      return $ "[h+" ++ show k ++ "]" ++ show w
    return $ h ++ r

isNullPtr :: Ptr a -> Bool
isNullPtr (Ptr addr#) = isTrue# (addr2Int# addr# ==# 0#)

nullPtr :: Ptr a
nullPtr = Ptr (int2Addr# 0#)

{-# NOINLINE _hide #-}
_hide :: a -> a
_hide x = x

ptrD :: Dest# a -> Ptr Word
ptrD d# = Ptr (unsafeCoerceAddr d#)
{-# INLINE ptrD #-}

-------------------------------------------------------------------------------
-- Helpers to extract values from type-level info given by Generic
-------------------------------------------------------------------------------

data DatatypeData = DatatypeData
  { dtypeName :: String,
    dtypeModName :: String,
    dtypePackageName :: String,
    dtypeIsNewType :: Bool
  }

getDatatypeData :: forall meta. (Datatype meta) => DatatypeData
getDatatypeData =
  DatatypeData
    { dtypeName = datatypeName @meta undefined,
      dtypeModName = moduleName @meta undefined,
      dtypePackageName = packageName @meta undefined,
      dtypeIsNewType = isNewtype @meta undefined
    }

data CtorData = CtorData {ctorName :: String, ctorFixity :: Fixity, ctorIsRecord :: Bool}

getCtorData :: forall meta. (Constructor meta) => CtorData
getCtorData =
  CtorData
    { ctorName = conName @meta undefined,
      ctorFixity = conFixity @meta undefined,
      ctorIsRecord = conIsRecord @meta undefined
    }

data SelectorData = SelectorData
  { selecName :: String,
    selecUnpackedness :: SourceUnpackedness,
    selecSrcStrictness :: SourceStrictness,
    selecFinalStrictness :: DecidedStrictness
  }

getSelectorData :: forall meta. (Selector meta) => SelectorData
getSelectorData =
  SelectorData
    { selecName = let n = selName @meta undefined in if null n then "" else "." ++ n ++ " ", -- TODO: detect when no sel and return Nothing
      selecUnpackedness = selSourceUnpackedness @meta undefined,
      selecSrcStrictness = selSourceStrictness @meta undefined,
      selecFinalStrictness = selDecidedStrictness @meta undefined
    }

-------------------------------------------------------------------------------
-- Tools to display compact region memory
-------------------------------------------------------------------------------

class ShowHeap (a :: Type) where
  _showHeap :: Int -> String -> a -> IO String

instance {-# OVERLAPPING #-} ShowHeap Int where
  _showHeap = _showHeapPrim "Int" 1 0

instance {-# OVERLAPPING #-} ShowHeap Char where
  _showHeap = _showHeapPrim "Char" 1 'a'

instance (Generic a, repA ~ Rep a (), ctors ~ GCtorsOf repA, ShowTryCtors ctors a) => ShowHeap a where
  _showHeap = _showTryCtors @ctors @a

_showHeapPrim :: String -> Int -> a -> (Int -> String -> a -> IO String)
_showHeapPrim typeName n representative indent selectorRepr x = do
  let pX = aToRawPtr x
  evaluatedInfoPtr <- let !r = representative in peekInfoPtr r
  actualInfoPtr <- peekInfoPtr x
  if actualInfoPtr == evaluatedInfoPtr
    then do
      rawX <- showRaw n x
      return $
        (replicate (2 * indent) ' ')
          ++ selectorRepr
          ++ "@"
          ++ show (ptrToWord pX)
          ++ " = [value] :: "
          ++ typeName
          ++ " "
          ++ rawX
    else do
      rawX <- showRaw 0 x
      return $
        (replicate (2 * indent) ' ')
          ++ "@"
          ++ show (ptrToWord pX)
          ++ " = THUNK :: "
          ++ typeName
          ++ " "
          ++ rawX

class ShowTryCtors (ctors :: [(Meta, [(Meta, Type)])]) (a :: Type) where
  _showTryCtors :: Int -> String -> a -> IO String

instance (Generic a, repA ~ Rep a (), metaA ~ GDatatypeMetaOf repA, Datatype metaA) => ShowTryCtors '[] a where
  _showTryCtors indent selectorRepr x = do
    let pAddr = ptrToWord $ aToRawPtr x
        DatatypeData {..} = getDatatypeData @metaA
    rawP <- showRaw 0 x
    return $
      (replicate (2 * indent) ' ')
        ++ selectorRepr
        ++ "@"
        ++ show pAddr
        ++ " = THUNK :: "
        ++ dtypeName
        ++ " "
        ++ rawP

instance (Generic a, repA ~ Rep a (), metaA ~ GDatatypeMetaOf repA, Datatype metaA, Constructor metaCtor, 'MetaCons symCtor f r ~ metaCtor, GShallow symCtor repA, ShowTryCtors otherCtors a, ShowFields fields, arity ~ Length fields, KnownNat arity) => ShowTryCtors ('(metaCtor, fields) : otherCtors) a where
  _showTryCtors indent selectorRepr x = do
    evaluatedInfoPtr <- getCtorInfoPtrFromSym @symCtor @a
    actualInfoPtr <- peekInfoPtr x
    if evaluatedInfoPtr == actualInfoPtr
      then do
        let pX = aToRawPtr x
            arity = fromInteger $ natVal (Proxy :: Proxy arity)
            DatatypeData {..} = getDatatypeData @metaA
            CtorData {..} = getCtorData @metaCtor
        rawP <- showRaw arity x
        next <- _showFields @fields (indent + 1) pX 0
        return $
          (replicate (2 * indent) ' ')
            ++ selectorRepr
            ++ "@"
            ++ show (ptrToWord pX)
            ++ " = "
            ++ ctorName
            ++ " "
            ++ (stimesMonoid arity "_ ")
            ++ ":: "
            ++ dtypeName
            ++ " "
            ++ rawP
            ++ "\n"
            ++ next
      else _showTryCtors @otherCtors @a indent selectorRepr x

class ShowFields (fields :: [(Meta, Type)]) where
  _showFields :: Int -> Ptr a -> Int -> IO String

instance ShowFields '[] where
  _showFields _ _ _ = return ""

instance (ShowHeap fieldType, ShowFields others, Selector metaSel) => ShowFields ('(metaSel, fieldType) : others) where
  _showFields indent pX fieldOffset = do
    let SelectorData {..} = getSelectorData @metaSel
    fieldAsWord <- peek $ pX `plusPtr` headerSize `plusPtr` (wordSize * fieldOffset)
    showField <- case wordToPtr' fieldAsWord :: Ptr' t of Ptr' field -> _showHeap @fieldType indent selecName field
    showNext <- _showFields @others indent pX (fieldOffset + 1)
    return $ showField ++ "\n" ++ showNext

showHeap :: forall a. (ShowHeap a) => a -> String
showHeap x = unsafePerformIO $ _showHeap @a 0 "" x

class GShallow (n :: Symbol) a where
  gShallowTerm :: a

instance (GShallow n (f p), GShallow n (g p)) => GShallow n ((f :*: g) p) where
  gShallowTerm = gShallowTerm @n @(f p) :*: gShallowTerm @n @(g p)

instance GShallow n (U1 p) where
  gShallowTerm = U1

instance GShallow n (K1 i c p) where
  gShallowTerm = K1 (unsafeCoerce placeholder :: c)

instance (GShallow n (f p)) => GShallow n (M1 i c f p) where
  gShallowTerm = M1 (gShallowTerm @n @(f p))

instance (b ~ IsJust (GSpecCtorOf symCtor (f p)), IfT b (GShallow symCtor (f p)) (GShallow symCtor (g p)), KnownBool b) => GShallow symCtor ((f :+: g) p) where
  gShallowTerm = ifV @b (L1 $ gShallowTerm @symCtor @(f p)) (R1 $ gShallowTerm @symCtor @(g p))

shallowTerm :: forall (symCtor :: Symbol) a. (Generic a, GShallow symCtor (Rep a ())) => a
shallowTerm = to @a $ gShallowTerm @symCtor @(Rep a ())

-------------------------------------------------------------------------------
-- Region and dests
-------------------------------------------------------------------------------

data FirstInhabitant = FirstInhabitant Int

firstInhabitant :: FirstInhabitant
firstInhabitant = FirstInhabitant 1234

newtype Region = Region {root :: Compact FirstInhabitant}

data RegionToken r where RegionToken :: Region -> RegionToken r

instance Consumable (RegionToken r) where
  consume (RegionToken _) = ()

instance Dupable (RegionToken r) where
  dupR (RegionToken c) = Moved (RegionToken c)

type RegionContext r = Reifies r Region

data Dest r a = Dest (Dest# a)

newtype Incomplete r a b = Incomplete (Dest# a -> State# RealWorld -> (# State# RealWorld, b #)) deriving (Data.Functor) via Data (Incomplete r a)

instance Control.Functor (Incomplete r a) where
  fmap f (Incomplete t) = Incomplete (\d# s -> case t d# s of (# s', b #) -> case f b of !r -> (# s', r #))

getRegion :: forall r. (RegionContext r) => Region
getRegion = reflect (Proxy :: Proxy r)
{-# INLINE getRegion #-}

withRegion :: forall b. (forall (r :: Type). (RegionContext r) => RegionToken r %1 -> Ur b) %1 -> Ur b
withRegion = toLinear _withRegion
{-# INLINE withRegion #-}

{-# NOINLINE _withRegion #-}
_withRegion :: forall b. (forall (r :: Type). (RegionContext r) => RegionToken r %1 -> Ur b) -> Ur b
_withRegion f =
  unsafePerformIO $ do
    c <- (compact firstInhabitant)
    let !firstInhabitantInRegion = getCompact c
        firstPtr = ptrToWord $ aToRawPtr $ firstInhabitantInRegion
    putDebugLn $
      "withRegion: allocating new region around @"
        ++ (show firstPtr)
    return $! reify (Region {root = c}) (\(proxy :: Proxy s) -> f (RegionToken @s (reflect proxy)))

fillComp :: forall r a b. (RegionContext r) => Incomplete r a b %1 -> Dest r a %1 -> b
fillComp = toLinear2 _fillComp
{-# INLINE fillComp #-}

fillLeaf :: forall r a. (RegionContext r) => a -> Dest r a %1 -> ()
fillLeaf x = fillComp (intoR (RegionToken @r (reflect (Proxy :: Proxy r))) x)
{-# INLINE fillLeaf #-}

_fillComp :: forall r a b. (RegionContext r) => Incomplete r a b -> Dest r a -> b
_fillComp (Incomplete f) (Dest d#) =
  unsafeDupablePerformIO . IO $ \s0 -> case putDebugLn# message (# s0, () #) of
    (# s1, () #) -> (f d# s1)
  where
    message =
      ( "fillComp: @"
          ++ (show . ptrToWord $ ptrD d#)
          ++ " will be supplied to Incomplete's payload"
      )

complete :: forall r a. (RegionContext r) => Incomplete r a () %1 -> Ur a
complete = toLinear _complete

completeExtract :: forall r a b. (RegionContext r) => Incomplete r a (Ur b) %1 -> Ur (a, b)
completeExtract = toLinear _completeExtract

-- TODO: should we add the redundant '(RegionContext r) =>' here?
intoR :: forall r a. RegionToken r %1 -> a -> Incomplete r a ()
intoR = toLinear2 _intoR

{-# INLINE _intoR #-}
_intoR :: forall r a. RegionToken r -> a -> Incomplete r a ()
_intoR (RegionToken (Region (Compact c# _ (MVar m#)))) x = Incomplete $
  \d# s0 -> case putInRegionIfNot# c# m# x s0 of
    (# s1, xInRegion #) -> case assign# d# xInRegion s1 of
      (# s2, pXInRegion #) -> putDebugLn# message (# s2, () #)
        where
          message =
            ( "intoR: @"
                ++ (show . ptrToWord $ ptrD d#)
                ++ " <- #"
                ++ (show . ptrToWord $ Ptr pXInRegion)
                ++ ": [value]"
            )

putInRegionIfNot# :: Compact# -> MVar# RealWorld () -> a -> State# RealWorld -> (# State# RealWorld, a #)
putInRegionIfNot# c# m# x = \s0 -> case compactContains# c# x s0 of
  (# s1, 1# #) -> (# s1, x #) -- already in region
  (# s1, _ #) -> case takeMVar# m# s1 of
    (# s2, () #) -> case compactAdd# c# x s2 of
      (# s3, xInRegion #) -> case putMVar# m# () s3 of
        s4 -> (# s4, xInRegion #)
{-# INLINE putInRegionIfNot# #-}

-- Ideally we could alloc just a cell, and not a full Ur here, as the Ur layer will be extracted and thrown away
{-# NOINLINE _completeExtract #-}
_completeExtract :: forall r a b. (RegionContext r) => Incomplete r a (Ur b) -> Ur (a, b)
_completeExtract (Incomplete f) = case getRegion @r of
  (Region (Compact c# _ (MVar m#))) -> unsafePerformIO . IO $ \s0 -> case takeMVar# m# s0 of
    (# s1, () #) -> case compactAddShallow# @(Ur (a, b)) c# (unsafeCoerceAddr (reflectInfoPtr# (# #) :: InfoPtrPlaceholder# 'Ur)) s1 of
      (# s2, rootReceiver #) -> case anyToAddr# rootReceiver s2 of
        (# s3, pRootReceiver #) -> case getSlots1# rootReceiver s3 of
          (# s4, (# d# #) #) -> case compactAddShallow# @(a, b) c# (unsafeCoerceAddr (reflectInfoPtr# (# #) :: InfoPtrPlaceholder# '(,))) s4 of
            (# s5, pair #) -> case assign# d# pair s5 of
              (# s6, pPair #) -> case getSlots2# pair s6 of
                (# s7, (# dRoot#, dCompanion# #) #) -> case putDebugLn# message ((# putMVar# m# () s7, () #)) of
                  (# s8, () #) -> case f dRoot# s8 of
                    (# s9, Ur companion #) -> case putInRegionIfNot# c# m# companion s9 of
                      (# s10, companionInReg #) -> case assign# dCompanion# companionInReg s10 of
                        (# s11, pCompanionInReg #) -> putDebugLn# message2 (# s11, _hide rootReceiver #)
                          where
                            message2 =
                              ( "completeExtract/end: "
                                  ++ (show . ptrToWord $ ptrD dCompanion#)
                                  ++ " <- #"
                                  ++ (show . ptrToWord $ Ptr pCompanionInReg)
                                  ++ ": [companion in reg]"
                              )
                  where
                    message =
                      ( "completeExtract/start: [region] <- #"
                          ++ (show . ptrToWord $ Ptr pRootReceiver)
                          ++ ": Ur _@"
                          ++ (show . ptrToWord $ ptrD d#)
                          ++ "\ncompleteExtract/start: @"
                          ++ (show . ptrToWord $ ptrD d#)
                          ++ " <- #"
                          ++ (show . ptrToWord $ Ptr pPair)
                          ++ ": (,) _@"
                          ++ (show . ptrToWord $ ptrD dRoot#)
                          ++ " _@"
                          ++ (show . ptrToWord $ ptrD dCompanion#)
                      )

{-# NOINLINE _complete #-}
_complete :: forall r a. (RegionContext r) => Incomplete r a () -> Ur a
_complete (Incomplete f) = case getRegion @r of
  (Region (Compact c# _ (MVar m#))) -> unsafePerformIO . IO $ \s0 -> case takeMVar# m# s0 of
    (# s1, () #) -> case compactAddShallow# @(Ur a) c# ((unsafeCoerceAddr (reflectInfoPtr# (# #) :: InfoPtrPlaceholder# 'Ur))) s1 of
      (# s2, rootReceiver #) -> case anyToAddr# rootReceiver s2 of
        (# s3, pRootReceiver #) -> case getSlots1# rootReceiver s3 of
          (# s4, (# d# #) #) -> case putDebugLn# message (# putMVar# m# () s4, () #) of
            (# s5, () #) -> case f d# s5 of
              (# s6, () #) -> (# s6, _hide rootReceiver #)
            where
              message =
                ( "complete: [region] <- #"
                    ++ (show . ptrToWord $ Ptr pRootReceiver)
                    ++ ": Ur _@"
                    ++ (show . ptrToWord $ ptrD d#)
                )

-- TODO: should we add the redundant '(RegionContext r) =>' here?
alloc :: forall r a. RegionToken r %1 -> Incomplete r a (Dest r a)
alloc = toLinear _alloc

{-# INLINE _alloc #-}
_alloc :: forall r a. RegionToken r -> Incomplete r a (Dest r a)
_alloc _ =
  Incomplete $
    \d# s0 ->
      (# s0, Dest d# #)

-------------------------------------------------------------------------------
-- Metaprogramming stuff for dests
-------------------------------------------------------------------------------

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

type family DestsOf liftedCtor (r :: Type) (a :: Type) where
  DestsOf liftedCtor r a = GDestsOf (LiftedCtorToSpecCtor liftedCtor a) r

class Fill liftedCtor (r :: Type) (a :: Type) where
  fill :: Dest r a -> DestsOf liftedCtor r a

instance (specCtor ~ LiftedCtorToSpecCtor liftedCtor a, GFill# liftedCtor specCtor a, DestsOf liftedCtor r a ~ GDestsOf specCtor r, RegionContext r) => Fill liftedCtor r a where
  fill :: Dest r a -> DestsOf liftedCtor r a
  fill (Dest d#) = case getRegion @r of
    (Region (Compact c# _ (MVar m#))) -> unsafeDupablePerformIO (IO (gFill# @liftedCtor @specCtor @a @r c# m# d#))
  {-# INLINE fill #-}

-- x must be already in the same region as the value, and fully evaluated
assign# :: Dest# a -> a -> State# RealWorld -> (# State# RealWorld, Addr# #)
assign# dest xInRegion s0 = case anyToAddr# xInRegion s0 of
  (# s1, pX #) -> let s2 = writeAddrOffAddr# (unsafeCoerceAddr dest) 0# pX s1 in (# s2, pX #)
{-# INLINE assign# #-}

showFill :: Ptr Word -> Ptr Word -> String -> [Ptr Word] -> String
showFill parentWriteLoc xAddr ctorName slots =
  "fill"
    ++ (show n)
    ++ ": @"
    ++ show (ptrToWord parentWriteLoc)
    ++ " <- #"
    ++ show (ptrToWord xAddr)
    ++ ": "
    ++ ctorName
    ++ " "
    ++ showSlots slots
  where
    n = length slots
    showSlots = intercalate " " . fmap showSlot
    showSlot ptr = "_@" ++ (show $ ptrToWord ptr)

-- ctor :: (Meta, [(Meta, Type)])
class GFill# liftedCtor (specCtor :: (Meta, [(Meta, Type)])) (a :: Type) where
  gFill# :: forall (r :: Type). Compact# -> MVar# RealWorld () -> Dest# a -> State# RealWorld -> (# State# RealWorld, GDestsOf specCtor r #)

instance (Generic a, repA ~ Rep a (), metaA ~ GDatatypeMetaOf repA, Datatype metaA, 'MetaCons symCtor fix hasSel ~ metaCtor, Constructor metaCtor, LiftedCtorToSymbol liftedCtor ~ symCtor, 'Just '(metaCtor, '[]) ~ GSpecCtorOf symCtor (Rep a ())) => GFill# liftedCtor '(metaCtor, '[]) a where
  gFill# :: forall (r :: Type). Compact# -> MVar# RealWorld () -> Dest# a -> State# RealWorld -> (# State# RealWorld, GDestsOf '(metaCtor, '[]) r #)
  gFill# c# m# dest s0 =
    case takeMVar# m# s0 of
      (# s1, () #) ->
        case compactAddShallow# c# (unsafeCoerceAddr (reflectInfoPtr# (# #) :: InfoPtrPlaceholder# liftedCtor)) s1 of
          (# s2, xInRegion #) -> case assign# dest xInRegion s2 of
            (# s3, addr# #) -> case putMVar# m# () s3 of
              s4 -> putDebugLn# (showFill (ptrD dest) (Ptr addr#) (ctorName $ getCtorData @metaCtor) []) (# s4, () #)
  {-# INLINE gFill# #-}

-- TODO: add constraints on ds_i variables to ensure no unpacking
instance (Generic a, repA ~ Rep a (), metaA ~ GDatatypeMetaOf repA, Datatype metaA, 'MetaCons symCtor fix hasSel ~ metaCtor, Constructor metaCtor, LiftedCtorToSymbol liftedCtor ~ symCtor, 'Just '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0)]) ~ GSpecCtorOf symCtor (Rep a ())) => GFill# liftedCtor '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0)]) a where
  gFill# :: forall (r :: Type). Compact# -> MVar# RealWorld () -> Dest# a -> State# RealWorld -> (# State# RealWorld, GDestsOf '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0)]) r #)
  gFill# c# m# dest s0 =
    case takeMVar# m# s0 of
      (# s1, () #) ->
        case compactAddShallow# c# (unsafeCoerceAddr (reflectInfoPtr# (# #) :: InfoPtrPlaceholder# liftedCtor)) s1 of
          (# s2, xInRegion #) -> case assign# dest xInRegion s2 of
            (# s3, addr# #) -> case getSlots1# xInRegion s3 of
              (# s4, (# d0# #) #) -> case putMVar# m# () s4 of
                s5 -> putDebugLn# (showFill (ptrD dest) (Ptr addr#) (ctorName $ getCtorData @metaCtor) [ptrD d0#]) (# s5, (Dest d0# :: Dest r t0) #)
  {-# INLINE gFill# #-}

-- TODO: add constraints on ds_i variables to ensure no unpacking
instance (Generic a, repA ~ Rep a (), metaA ~ GDatatypeMetaOf repA, Datatype metaA, 'MetaCons symCtor fix hasSel ~ metaCtor, Constructor metaCtor, LiftedCtorToSymbol liftedCtor ~ symCtor, 'Just '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1)]) ~ GSpecCtorOf symCtor (Rep a ())) => GFill# liftedCtor '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1)]) a where
  gFill# :: forall (r :: Type). Compact# -> MVar# RealWorld () -> Dest# a -> State# RealWorld -> (# State# RealWorld, GDestsOf '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1)]) r #)
  gFill# c# m# dest s0 =
    case takeMVar# m# s0 of
      (# s1, () #) ->
        case compactAddShallow# c# (unsafeCoerceAddr (reflectInfoPtr# (# #) :: InfoPtrPlaceholder# liftedCtor)) s1 of
          (# s2, xInRegion #) -> case assign# dest xInRegion s2 of
            (# s3, addr# #) -> case getSlots2# xInRegion s3 of
              (# s4, (# d0#, d1# #) #) -> case putMVar# m# () s4 of
                s5 -> putDebugLn# (showFill (ptrD dest) (Ptr addr#) (ctorName $ getCtorData @metaCtor) [ptrD d0#, ptrD d1#]) (# s5, (Dest d0# :: Dest r t0, Dest d1# :: Dest r t1) #)
  {-# INLINE gFill# #-}

-- TODO: add constraints on ds_i variables to ensure no unpacking
instance (Generic a, repA ~ Rep a (), metaA ~ GDatatypeMetaOf repA, Datatype metaA, 'MetaCons symCtor fix hasSel ~ metaCtor, Constructor metaCtor, LiftedCtorToSymbol liftedCtor ~ symCtor, 'Just '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1), '( 'MetaSel f2 u2 ss2 ds2, t2)]) ~ GSpecCtorOf symCtor (Rep a ())) => GFill# liftedCtor '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1), '( 'MetaSel f2 u2 ss2 ds2, t2)]) a where
  gFill# :: forall (r :: Type). Compact# -> MVar# RealWorld () -> Dest# a -> State# RealWorld -> (# State# RealWorld, GDestsOf '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1), '( 'MetaSel f2 u2 ss2 ds2, t2)]) r #)
  gFill# c# m# dest s0 =
    case takeMVar# m# s0 of
      (# s1, () #) ->
        case compactAddShallow# c# (unsafeCoerceAddr (reflectInfoPtr# (# #) :: InfoPtrPlaceholder# liftedCtor)) s1 of
          (# s2, xInRegion #) -> case assign# dest xInRegion s2 of
            (# s3, addr# #) -> case getSlots3# xInRegion s3 of
              (# s4, (# d0#, d1#, d2# #) #) -> case putMVar# m# () s4 of
                s5 -> putDebugLn# (showFill (ptrD dest) (Ptr addr#) (ctorName $ getCtorData @metaCtor) [ptrD d0#, ptrD d1#, ptrD d2#]) (# s5, (Dest d0# :: Dest r t0, Dest d1# :: Dest r t1, Dest d2# :: Dest r t2) #)
  {-# INLINE gFill# #-}

-- TODO: add constraints on ds_i variables to ensure no unpacking
instance (Generic a, repA ~ Rep a (), metaA ~ GDatatypeMetaOf repA, Datatype metaA, 'MetaCons symCtor fix hasSel ~ metaCtor, Constructor metaCtor, LiftedCtorToSymbol liftedCtor ~ symCtor, 'Just '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1), '( 'MetaSel f2 u2 ss2 ds2, t2), '( 'MetaSel f3 u3 ss3 ds3, t3)]) ~ GSpecCtorOf symCtor (Rep a ())) => GFill# liftedCtor '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1), '( 'MetaSel f2 u2 ss2 ds2, t2), '( 'MetaSel f3 u3 ss3 ds3, t3)]) a where
  gFill# :: forall (r :: Type). Compact# -> MVar# RealWorld () -> Dest# a -> State# RealWorld -> (# State# RealWorld, GDestsOf '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1), '( 'MetaSel f2 u2 ss2 ds2, t2), '( 'MetaSel f3 u3 ss3 ds3, t3)]) r #)
  gFill# c# m# dest s0 =
    case takeMVar# m# s0 of
      (# s1, () #) ->
        case compactAddShallow# c# (unsafeCoerceAddr (reflectInfoPtr# (# #) :: InfoPtrPlaceholder# liftedCtor)) s1 of
          (# s2, xInRegion #) -> case assign# dest xInRegion s2 of
            (# s3, addr# #) -> case getSlots4# xInRegion s3 of
              (# s4, (# d0#, d1#, d2#, d3# #) #) -> case putMVar# m# () s4 of
                s5 -> putDebugLn# (showFill (ptrD dest) (Ptr addr#) (ctorName $ getCtorData @metaCtor) [ptrD d0#, ptrD d1#, ptrD d2#, ptrD d3#]) (# s5, (Dest d0# :: Dest r t0, Dest d1# :: Dest r t1, Dest d2# :: Dest r t2, Dest d3# :: Dest r t3) #)
  {-# INLINE gFill# #-}

-- TODO: add constraints on ds_i variables to ensure no unpacking
instance (Generic a, repA ~ Rep a (), metaA ~ GDatatypeMetaOf repA, Datatype metaA, 'MetaCons symCtor fix hasSel ~ metaCtor, Constructor metaCtor, LiftedCtorToSymbol liftedCtor ~ symCtor, 'Just '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1), '( 'MetaSel f2 u2 ss2 ds2, t2), '( 'MetaSel f3 u3 ss3 ds3, t3), '( 'MetaSel f4 u4 ss4 ds4, t4)]) ~ GSpecCtorOf symCtor (Rep a ())) => GFill# liftedCtor '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1), '( 'MetaSel f2 u2 ss2 ds2, t2), '( 'MetaSel f3 u3 ss3 ds3, t3), '( 'MetaSel f4 u4 ss4 ds4, t4)]) a where
  gFill# :: forall (r :: Type). Compact# -> MVar# RealWorld () -> Dest# a -> State# RealWorld -> (# State# RealWorld, GDestsOf '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1), '( 'MetaSel f2 u2 ss2 ds2, t2), '( 'MetaSel f3 u3 ss3 ds3, t3), '( 'MetaSel f4 u4 ss4 ds4, t4)]) r #)
  gFill# c# m# dest s0 =
    case takeMVar# m# s0 of
      (# s1, () #) ->
        case compactAddShallow# c# (unsafeCoerceAddr (reflectInfoPtr# (# #) :: InfoPtrPlaceholder# liftedCtor)) s1 of
          (# s2, xInRegion #) -> case assign# dest xInRegion s2 of
            (# s3, addr# #) -> case getSlots5# xInRegion s3 of
              (# s4, (# d0#, d1#, d2#, d3#, d4# #) #) -> case putMVar# m# () s4 of
                s5 -> putDebugLn# (showFill (ptrD dest) (Ptr addr#) (ctorName $ getCtorData @metaCtor) [ptrD d0#, ptrD d1#, ptrD d2#, ptrD d3#, ptrD d4#]) (# s5, (Dest d0# :: Dest r t0, Dest d1# :: Dest r t1, Dest d2# :: Dest r t2, Dest d3# :: Dest r t3, Dest d4# :: Dest r t4) #)
  {-# INLINE gFill# #-}

-- TODO: add constraints on ds_i variables to ensure no unpacking
instance (Generic a, repA ~ Rep a (), metaA ~ GDatatypeMetaOf repA, Datatype metaA, 'MetaCons symCtor fix hasSel ~ metaCtor, Constructor metaCtor, LiftedCtorToSymbol liftedCtor ~ symCtor, 'Just '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1), '( 'MetaSel f2 u2 ss2 ds2, t2), '( 'MetaSel f3 u3 ss3 ds3, t3), '( 'MetaSel f4 u4 ss4 ds4, t4), '( 'MetaSel f5 u5 ss5 ds5, t5)]) ~ GSpecCtorOf symCtor (Rep a ())) => GFill# liftedCtor '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1), '( 'MetaSel f2 u2 ss2 ds2, t2), '( 'MetaSel f3 u3 ss3 ds3, t3), '( 'MetaSel f4 u4 ss4 ds4, t4), '( 'MetaSel f5 u5 ss5 ds5, t5)]) a where
  gFill# :: forall (r :: Type). Compact# -> MVar# RealWorld () -> Dest# a -> State# RealWorld -> (# State# RealWorld, GDestsOf '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1), '( 'MetaSel f2 u2 ss2 ds2, t2), '( 'MetaSel f3 u3 ss3 ds3, t3), '( 'MetaSel f4 u4 ss4 ds4, t4), '( 'MetaSel f5 u5 ss5 ds5, t5)]) r #)
  gFill# c# m# dest s0 =
    case takeMVar# m# s0 of
      (# s1, () #) ->
        case compactAddShallow# c# (unsafeCoerceAddr (reflectInfoPtr# (# #) :: InfoPtrPlaceholder# liftedCtor)) s1 of
          (# s2, xInRegion #) -> case assign# dest xInRegion s2 of
            (# s3, addr# #) -> case getSlots6# xInRegion s3 of
              (# s4, (# d0#, d1#, d2#, d3#, d4#, d5# #) #) -> case putMVar# m# () s4 of
                s5 -> putDebugLn# (showFill (ptrD dest) (Ptr addr#) (ctorName $ getCtorData @metaCtor) [ptrD d0#, ptrD d1#, ptrD d2#, ptrD d3#, ptrD d4#, ptrD d5#]) (# s5, (Dest d0# :: Dest r t0, Dest d1# :: Dest r t1, Dest d2# :: Dest r t2, Dest d3# :: Dest r t3, Dest d4# :: Dest r t4, Dest d5# :: Dest r t5) #)
  {-# INLINE gFill# #-}

-- TODO: add constraints on ds_i variables to ensure no unpacking
instance (Generic a, repA ~ Rep a (), metaA ~ GDatatypeMetaOf repA, Datatype metaA, 'MetaCons symCtor fix hasSel ~ metaCtor, Constructor metaCtor, LiftedCtorToSymbol liftedCtor ~ symCtor, 'Just '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1), '( 'MetaSel f2 u2 ss2 ds2, t2), '( 'MetaSel f3 u3 ss3 ds3, t3), '( 'MetaSel f4 u4 ss4 ds4, t4), '( 'MetaSel f5 u5 ss5 ds5, t5), '( 'MetaSel f6 u6 ss6 ds6, t6)]) ~ GSpecCtorOf symCtor (Rep a ())) => GFill# liftedCtor '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1), '( 'MetaSel f2 u2 ss2 ds2, t2), '( 'MetaSel f3 u3 ss3 ds3, t3), '( 'MetaSel f4 u4 ss4 ds4, t4), '( 'MetaSel f5 u5 ss5 ds5, t5), '( 'MetaSel f6 u6 ss6 ds6, t6)]) a where
  gFill# :: forall (r :: Type). Compact# -> MVar# RealWorld () -> Dest# a -> State# RealWorld -> (# State# RealWorld, GDestsOf '(metaCtor, '[ '( 'MetaSel f0 u0 ss0 ds0, t0), '( 'MetaSel f1 u1 ss1 ds1, t1), '( 'MetaSel f2 u2 ss2 ds2, t2), '( 'MetaSel f3 u3 ss3 ds3, t3), '( 'MetaSel f4 u4 ss4 ds4, t4), '( 'MetaSel f5 u5 ss5 ds5, t5), '( 'MetaSel f6 u6 ss6 ds6, t6)]) r #)
  gFill# c# m# dest s0 =
    case takeMVar# m# s0 of
      (# s1, () #) ->
        case compactAddShallow# c# (unsafeCoerceAddr (reflectInfoPtr# (# #) :: InfoPtrPlaceholder# liftedCtor)) s1 of
          (# s2, xInRegion #) -> case assign# dest xInRegion s2 of
            (# s3, addr# #) -> case getSlots7# xInRegion s3 of
              (# s4, (# d0#, d1#, d2#, d3#, d4#, d5#, d6# #) #) -> case putMVar# m# () s4 of
                s5 -> putDebugLn# (showFill (ptrD dest) (Ptr addr#) (ctorName $ getCtorData @metaCtor) [ptrD d0#, ptrD d1#, ptrD d2#, ptrD d3#, ptrD d4#, ptrD d5#, ptrD d6#]) (# s5, (Dest d0# :: Dest r t0, Dest d1# :: Dest r t1, Dest d2# :: Dest r t2, Dest d3# :: Dest r t3, Dest d4# :: Dest r t4, Dest d5# :: Dest r t5, Dest d6# :: Dest r t6) #)
  {-# INLINE gFill# #-}

type family Length (a :: [k]) :: Nat where
  Length '[] = 0
  Length (_ : xs) = 1 + Length xs
  Length _ = TypeError ('Text "No match for Length")

type family (a :: [k]) ++ (b :: [k]) :: [k] where
  '[] ++ y = y
  (x : xs) ++ y = x : (xs ++ y)
  _ ++ _ = TypeError ('Text "No match for ++")

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

type family GCtorsOf (repA :: Type) :: [(Meta, [(Meta, Type)])] where
  GCtorsOf (C1 meta f p) = '[ '(meta, GFieldsOf (f p))]
  GCtorsOf (M1 _ _ f p) = GCtorsOf (f p)
  GCtorsOf ((f :+: g) p) = GCtorsOf (f p) ++ GCtorsOf (g p)
  GCtorsOf _ = TypeError ('Text "No match for GCtorsOf")

type family GSpecCtorOf (symCtor :: Symbol) (repA :: Type) :: Maybe (Meta, [(Meta, Type)]) where
  -- GSpecCtorOf "leaf" _ = 'Just '( ('MetaCons "leaf" 'PrefixI 'False), '[])
  -- GSpecCtorOf "comp" _ = 'Just '( ('MetaCons "comp" 'PrefixI 'False), '[])
  GSpecCtorOf symCtor (C1 ('MetaCons symCtor x y) f p) = 'Just '(('MetaCons symCtor x y), GFieldsOf (f p))
  GSpecCtorOf symCtor (C1 ('MetaCons _ _ _) _ _) = 'Nothing
  GSpecCtorOf symCtor ((f :+: g) p) = GSpecCtorOf symCtor (f p) <|> GSpecCtorOf symCtor (g p)
  GSpecCtorOf symCtor (V1 _) = 'Nothing
  GSpecCtorOf symCtor (M1 _ _ f p) = GSpecCtorOf symCtor (f p)
  GSpecCtorOf _ _ = TypeError ('Text "No match for GHasCtor")

type family LiftedCtorToSpecCtor liftedCtor (a :: Type) :: (Meta, [(Meta, Type)]) where
  LiftedCtorToSpecCtor liftedCtor a = FromJust (GSpecCtorOf (LiftedCtorToSymbol liftedCtor) (Rep a ()))

type family IsJust (x :: Maybe k) :: Bool where
  IsJust ('Just v) = 'True
  IsJust 'Nothing = 'False

type family FromJust (x :: Maybe k) :: k where
  FromJust ('Just v) = v
  FromJust 'Nothing = TypeError ('Text "FromJust error: got 'Nothing")

type family (x :: Maybe k) <|> (y :: Maybe k) :: Maybe k where
  ('Just v) <|> _ = 'Just v
  'Nothing <|> y = y

type family (a :: Bool) || (b :: Bool) :: Bool where
  'False || b = b
  'True || _ = 'True

type family IfT (a :: Bool) (x :: k) (y :: k) :: k where
  IfT 'True x _ = x
  IfT 'False _ y = y

class KnownBool (b :: Bool) where
  ifV :: ((b ~ 'True) => a) -> ((b ~ 'False) => a) -> a

instance KnownBool 'True where
  ifV t _ = t

instance KnownBool 'False where
  ifV _ f = f
