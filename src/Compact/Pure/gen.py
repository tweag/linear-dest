template = """
-- TODO: add constraints on ds_i variables to ensure no unpacking
instance (Generic a, repA ~ Rep a (), metaA ~ GDatatypeMetaOf repA, Datatype metaA, 'MetaCons symCtor fix hasSel ~ metaCtor, Constructor metaCtor, LiftedCtorToSymbol liftedCtor ~ symCtor, 'Just '(metaCtor, '[ {metaSelList}]) ~ GSpecCtorOf symCtor (Rep a ())) => GFill# liftedCtor '(metaCtor, '[ {metaSelList}]) a where
  gFill# :: forall (r :: Type). Compact# -> MVar# RealWorld () -> Dest# a -> State# RealWorld -> (# State# RealWorld, GDestsOf '(metaCtor, '[ {metaSelList}]) r #)
  gFill# c# m# dest s0 =
    case takeMVar# m# s0 of
      (# s1, () #) ->
        case compactAddShallow# c# (unsafeCoerceAddr (reflectInfoPtr# (# #) :: InfoPtrPlaceholder# liftedCtor)) s1 of
          (# s2, xInRegion #) -> case affect# dest xInRegion s2 of
            (# s3, addr# #) -> case getSlots{n}# xInRegion s3 of
              (# s4, (# {destPrimList} #) #) -> case putMVar# m# () s4 of
                s5 -> putDebugLn# (showFill (ptrD dest) (Ptr addr#) (ctorName $ getCtorData @metaCtor) [{ptrDList}]) (# s5, ({destLiftedList}) #)
  {{-# INLINE gFill# #-}}"""

for n in range(1, 7+1):
    metaSelList = ", ".join(["'( 'MetaSel f{} u{} ss{} ds{}, t{})".format(i, i, i, i, i) for i in range(n)])
    destPrimList = ", ".join(["d{}#".format(i) for i in range(n)])
    destLiftedList = ", ".join(["Dest d{}# :: Dest r t{}".format(i, i) for i in range(n)])
    ptrDList = ", ".join(["ptrD d{}#".format(i) for i in range(n)])
    print(template.format(n=n, destPrimList=destPrimList, ptrDList=ptrDList, metaSelList=metaSelList, destLiftedList=destLiftedList))
    print()

for n in range(1, 7 + 1):
    r = ", ".join("'(mS{}, t{})".format(i, i) for i in range(n))
    print(r)
