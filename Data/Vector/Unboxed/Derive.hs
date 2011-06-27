{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, TemplateHaskell, FlexibleInstances, TypeSynonymInstances, TypeOperators #-}

module Data.Vector.Unboxed.Derive
  (
-- | This module provides the template haskell functions 'deriveMany' and
-- 'deriveOne' for deriving 'Unbox' instances. The following two examples should cover the most common use cases:
-- 
-- @{-\# LANGUAGE TemplateHaskell, TypeFamilies, MultiParamTypeClasses #-} -- required language features
--
-- data Pair a b = Pair a b
--'deriveOne' ''Pair 'Full'
--
-- data ComplicatedType = A | B (Int -> Int)
--'deriveOne' ''ComplicatedType 'StrictBoxed'@
--
-- The first case converts the type into a tuple, generating an instance
--
-- @instance ('U.Unbox' a, 'U.Unbox' b) => 'U.Unbox' (Pair a b)@
-- 
-- The second case represents the type boxed, but strict (as is consistent with the 'unbox' claim), and generates an instance
--
-- @instance 'U.Unbox' ComplicatedType@
--
-- This second representation uses "Data.Vector.Strict" internally.
--
-- The derive statments could be joined together:
--
-- @'deriveMany' [''Pair :~ 'Full', ''ComplicatedType :~ 'StrictBoxed']@
--
--
   SimpleRepr(..),
   Repr(..),
   MVectorRep,
   DeriveStyle(..),
   (:~)(..),
   deriveMany,
   deriveOne,
  )
  where

import Control.Monad
import Language.Haskell.TH
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Strict as S
import Data.Word(Word8,Word)
-- | Customise the element representation
class SimpleRepr a where
    type ElemRep a
    {-# INLINE toRep #-}
    toRep :: a -> ElemRep a
    {-# INLINE fromRep #-}
    fromRep :: ElemRep a -> a

type MVectorRep a = G.Mutable (VectorRep a)

-- | Customise the element /and/ vector representation (eg for boxed representations)
class SimpleRepr a => Repr a where
    type VectorRep a :: * -> *
    {-# INLINE wrapR #-}
    wrapR :: VectorRep a (ElemRep a) -> U.Vector a
    {-# INLINE unwrapR #-}
    unwrapR :: U.Vector a -> VectorRep a (ElemRep a)
    {-# INLINE mwrapR #-}
    mwrapR :: MVectorRep a s (ElemRep a) -> U.MVector s a
    {-# INLINE munwrapR #-}
    munwrapR :: U.MVector s a -> MVectorRep a s (ElemRep a)

  --(runIO . putStrLn . show) =<< instanceSplice (ConT nm)
--  runIO . putStrLn . pprint $ PragmaD (InlineP (mkName "hello") (InlineSpec True False Nothing))
--   undefined

data DeriveStyle 
  = Full -- ^ Fully automatic, represented as tuple
  | StrictBoxed -- ^ Fully automatic, represented as boxed (strict)
  | LazyBoxed -- ^ Fully automatic, represented as boxed (lazy)
  | Enum -- ^ Fully automatic, represented as Word8 using enum instance (TODO: other types
  | CustomRepr { needArgsUnboxed :: Bool } -- ^ User provides a 'SimpleRepr' instance (i.e. a custom representation of elements)
  | CustomVector { needArgsUnboxed :: Bool } -- ^ User provides instances of 'SimpleRepr' and 'Repr' (i.e. a custom vector representation, too)

data a :~ b = a :~ b

-- | Multiple-type version of 'deriveOne'
deriveMany :: [Name :~ DeriveStyle] -> Q [Dec]
deriveMany nms = do
    dss <- mapM (\(nm:~sty) -> deriveOne nm sty) nms
    return (concat dss)

-- | The main driver.
deriveOne nm Full = fullDerive True nm
deriveOne nm StrictBoxed = fullDerive False nm
deriveOne nm LazyBoxed = reportError $ "LazyBoxed not implemented yet. Name " ++ show nm --fullDerive False 
deriveOne nm Enum = deriveEnum nm
deriveOne nm (CustomRepr b) = reprDerive nm b
deriveOne nm (CustomVector b) = minDerive nm b
  

fullDerive unboxed nm = do
   (ty,tyVars,cxt,cons) <- getTypeAndVars nm
   sri <- (if unboxed then unboxedSimpleReprInstance else boxedSimpleReprInstance) ty tyVars cxt cons
   ds <- reprDerive' ty tyVars cxt unboxed
   return (sri ++ ds)

deriveEnum nm = do
   (ty,tyVars,cxt,cons) <- getTypeAndVars nm
   case (tyVars,cxt) of
       ([],[]) -> return ()
       _       -> reportError $ "Can't handle enums with arguments or context. Type " ++ show nm
   sri <- enumSimpleReprInstance ty (length cons)
   ds <- reprDerive' ty tyVars cxt False
   return (sri ++ ds)

reprDerive nm unboxInternals = do
    (ty,tyVars,cxt,_) <- getTypeAndVars nm
    reprDerive' ty tyVars cxt unboxInternals

reprDerive' ty tyVars cxt unboxInternals = do
    rs <- reprInstance ty tyVars cxt
    ds <- minDerive' ty tyVars cxt unboxInternals
    return (rs ++ ds)

minDerive nm unboxInternals = do
    (ty,tyVars,cxt,_) <- getTypeAndVars nm
    minDerive' ty tyVars cxt unboxInternals

minDerive' ty tyVars cxt unboxInternals = do
    mi <- mvectorInstance ty tyVars cxt unboxInternals
    i <- vectorInstance ty tyVars cxt unboxInternals
    ui <- unboxInstance ty tyVars cxt unboxInternals
    return [mi,i,ui]
 
-- deriveInstance nm = do
--     let ty = ConT nm
--     mi <- mvectorInstance ty
--     i <- vectorInstance ty
--     rs <- reprInstance ty
--     return (rs ++ [i,mi])

appTy t ts = foldl AppT (ConT t) ts
appTy' t ts = foldl AppT t ts
app v exps = app' (VarE v) exps
app' v exps = foldl AppE v exps
var v = VarE v
val name e = [ValD (VarP name) (NormalB e) [], inline (nameBase name)]
fun name vars exp = [FunD name [Clause (map VarP vars) (NormalB exp) []], inline (nameBase name)]
inline name = PragmaD (InlineP (mkName name) (InlineSpec True False Nothing))

reportError msg = fail ("derive-vector: " ++ msg)

-- | Looks up the type. Returns: the fully-applied type, the variables to which it is applied, the context on those variables, the constructors
getTypeAndVars :: Name -> Q (Type, [Name], Cxt, [Con])
getTypeAndVars nm = do
    let extractTyVars bndrs = map (\bndr -> case bndr of { PlainTV n -> n; KindedTV n _ -> n }) bndrs
        applyType bndrs = appTy nm (map VarT (extractTyVars bndrs))

    i <- reify nm
    case i of
        TyConI d -> case d of
            NewtypeD cxt _nm bndrs con _deriving -> return (applyType bndrs, extractTyVars bndrs, cxt, [con])
            DataD cxt _nm bndrs cons _deriving -> return (applyType bndrs, extractTyVars bndrs, cxt, cons)
            -- hack:
            FamilyD DataFam _nm bndrs _ -> return (applyType bndrs, extractTyVars bndrs, [], [])
            p -> reportError $ "expected newtype, datatype, or data family, but got " ++ (show p)
        p -> reportError $ "expected a type, but got " ++ (show p)

enumSimpleReprInstance ty n = do
    let reprType = if n == 2 then ''Bool else if n <= 256 then ''Word8 else ''Word 
    --when (n > 256) $ reportError ("Enums with more than 256 constructors not yet supported. Type: " ++ show ty)
    x <- newName "x"
    return [InstanceD [] (appTy ''SimpleRepr [ty])
            (concat [[TySynInstD ''ElemRep [ty] (ConT reprType)],
                     fun 'toRep [x] (app 'toEnum [app 'fromEnum [var x]]),
                     fun 'fromRep [x] (app 'toEnum [app 'fromEnum [var x]])])]

-- | Generate the 'SimpleRepr' as 'ElemRep a = StrictBox a'.
boxedSimpleReprInstance ty tyVars _cxt _cons = do
    return [InstanceD [] (appTy ''SimpleRepr [ty])
              (concat [[TySynInstD ''ElemRep [ty] (appTy ''StrictBox [ty])],
               fun 'toRep [] (ConE 'StrictBox),
               fun 'fromRep [] (VarE 'unStrictBox)])]

-- | Generate the 'SimpleRepr' instance by converting to a tuple
unboxedSimpleReprInstance ty tyVars cxt cons = do
    con <- case cons of
       [x] -> return x
       _  -> reportError ("datatype must have exactly one constructor. Type: " ++ show ty)
    
    -- get the constructor name, and the types of the constructor fields
    (conNm, fieldTys) <- case con of
         NormalC conNm fs -> return $ (conNm, map snd fs)
         RecC conNm fs -> return $ (conNm, map (\(a,b,c) -> c) fs)
         InfixC f conNm g -> return $ (conNm, map snd [f,g])
         ForallC _ _ _ -> reportError ("existential types not supported. Type " ++ show ty)
    
    -- make the variables
    vars <- mapM (newName . (:[])) (take (length fieldTys) ['a'..])

    -- Converts the list @xs@ to a tree like @tuple [singleton x, singleton y, tuple [singleton z]]@
    -- where tuple only accepts lengths 0,2,3,4,5,6 (the tuple instances of 'Data.Vector.Unboxed.Unbox').
    -- We use this to generate tuple types, patterns, and expressions.
    let tupler :: ([tup] -> tup) -> (a -> tup) -> [a] -> tup
        tupler tuple singleton [x] = singleton x
        tupler tuple singleton xs | length xs <= 6 =  tuple . map singleton $ xs
                                  | otherwise = case splitAt (length xs `div` 2) xs of
                                      (l,r) -> tuple [tupler tuple singleton l, tupler tuple singleton r]

    -- the tuple type
    let repTy = tupler (\xs -> appTy' (TupleT (length xs)) xs) id fieldTys
    -- the tuple expression
        tupExp = tupler TupE VarE vars
    -- the tuple pattern
        tupPat = tupler TupP VarP vars


    -- the instance header
    let instTy = appTy ''SimpleRepr [ty]

    let decls = 
           [
             -- the type family instance
             TySynInstD ''ElemRep [ty] repTy,
             -- the converters
             inline "toRep",
             FunD 'toRep [Clause [ConP conNm (map VarP vars)] (NormalB tupExp) []],
             inline "fromRep",
             FunD 'fromRep [Clause [tupPat] (NormalB (app' (ConE conNm) (map VarE vars))) []]
           ]

    -- we copy the constructor context here (normally, this would be empty)
    return [InstanceD cxt instTy decls]
            
    

-- | Generates the 'Repr' instance, from the 'SimpleRepr' instance. Also generate the \'newtype instances\'.
reprInstance t _tyVars cxt = do
    [wrapRName, unwrapRName, mwrapRName, munwrapRName,s] <- mapM newName ["Vector", "unVector", "MVector", "unMVector", "s"]
    
    let 
    -- newtype instance Vector $t = Vector { unVector :: U.Vector (ElemRep $t) }
        vectorNewtype = NewtypeInstD [] ''U.Vector [t] (RecC wrapRName [(unwrapRName, NotStrict, AppT (ConT ''U.Vector) (AppT (ConT ''ElemRep) t))]) []
    -- newtype instance MVector s $t = MVector { unMVector :: U.MVector s (ElemRep $t) }
        mvectorNewtype = NewtypeInstD [] ''U.MVector [VarT s,t] (RecC mwrapRName [(munwrapRName, NotStrict, AppT (AppT (ConT ''U.MVector) (VarT s)) (AppT (ConT ''ElemRep) t))]) []

    -- now generate the 'Repr' instance
    let reprInstanceTy = AppT (ConT ''Repr) t
        decls = concat
          [
           -- type VectorRep $t = U.Vector
           [TySynInstD ''VectorRep [t] (ConT ''U.Vector)],
           val 'wrapR (ConE wrapRName),
           val 'unwrapR (var unwrapRName),
           val 'mwrapR (ConE mwrapRName),
           val 'munwrapR (var munwrapRName)
          ]
    return [vectorNewtype, mvectorNewtype, InstanceD cxt reprInstanceTy decls]

-- | A (Unbox a) requirement for each type variable a (not always right, but easiest and close enough).
unboxCxt tyVars unboxInternals = if unboxInternals then map (\tyVar -> ClassP ''U.Unbox [VarT tyVar]) tyVars else []

-- | Generates the 'GM.MVector' instance.
mvectorInstance t tyVars cxt unboxInternals = do
  -- make the instance head:
  -- instance GM.MVector MVector t
  let mInstanceTy = AppT (AppT (ConT ''GM.MVector) (ConT ''U.MVector)) t

  -- make the basic variables
  [v, w, m, n, a] <- mapM newName ["v", "w", "m", "n", "a"]

  -- make the helper functions
  let 
      wrap e = app 'mwrapR [e]
      wrapM e = app 'liftM [var 'mwrapR, e]
      unwrap e = app 'munwrapR [e]
      wrapElem e = app 'fromRep [e]
      wrapElemM e = app 'liftM [var 'fromRep, e]
      unwrapElem e = app 'toRep [e]

  -- make the functions
  let
      decls = concat 
          [
           fun 'GM.basicLength [v] (app 'GM.basicLength [unwrap (var v)]),
           fun 'GM.basicUnsafeSlice [m,n,v] (wrap (app 'GM.basicUnsafeSlice [var m,var n, unwrap (var v)])),
           fun 'GM.basicOverlaps [v,w] (app 'GM.basicOverlaps [unwrap (var v), unwrap (var w)]),
           fun 'GM.basicUnsafeNew [n] (wrapM (app 'GM.basicUnsafeNew [var n])),
           fun 'GM.basicUnsafeReplicate [n,a] (wrapM (app 'GM.basicUnsafeReplicate [var n, unwrapElem (var a)])),
           fun 'GM.basicUnsafeRead [v,n] (wrapElemM (app 'GM.basicUnsafeRead [unwrap (var v), var n])),
           fun 'GM.basicUnsafeWrite [v,n,a] (app 'GM.basicUnsafeWrite [unwrap (var v), var n, unwrapElem (var a)]),
           fun 'GM.basicClear [v] (app 'GM.basicClear [unwrap (var v)]),
           fun 'GM.basicSet [v,a] (app 'GM.basicSet [unwrap (var v), unwrapElem (var a)]),
           fun 'GM.basicUnsafeCopy [v,w] (app 'GM.basicUnsafeCopy [unwrap (var v), unwrap (var w)]),
           fun 'GM.basicUnsafeGrow [v,n] (wrapM (app 'GM.basicUnsafeGrow [unwrap (var v), var n]))
          ]
  
  return (InstanceD (cxt ++ unboxCxt tyVars unboxInternals) mInstanceTy decls)

-- | Generates the 'G.Vector' instance
vectorInstance t tyVars cxt unboxInternals = do
    -- instance G.Vector Vector t
    let instanceTy = AppT (AppT (ConT ''G.Vector) (ConT ''U.Vector)) t

    -- make the variables
    [v, w, m, n, a, b] <- mapM newName ["v", "w", "m", "n", "a", "b"]

    let 
        wrap e = app 'wrapR [e]
        wrapM e = app 'liftM [var 'wrapR, e]
        wrapMutM e = app 'liftM [var 'mwrapR, e]
        unwrap e = app 'unwrapR [e]
        unwrapMut e = app 'munwrapR [e]
        wrapElem e = app 'fromRep [e]
        wrapElemM e = app 'liftM [var 'fromRep, e]
        unwrapElem e = app 'toRep [e]

    let 
        decls = concat
            [
              fun 'G.basicUnsafeFreeze [v] (wrapM (app 'G.basicUnsafeFreeze [unwrapMut (var v)])),
              fun 'G.basicUnsafeThaw [v] (wrapMutM (app 'G.basicUnsafeThaw [unwrap (var v)])),
              fun 'G.basicLength [v] (app 'G.basicLength [unwrap (var v)]),
              fun 'G.basicUnsafeSlice [m,n,v] (wrap (app 'G.basicUnsafeSlice [var m, var n, unwrap (var v)])),
              fun 'G.basicUnsafeIndexM [v,n] (wrapElemM (app 'G.basicUnsafeIndexM [unwrap (var v), var n])),
              fun 'G.basicUnsafeCopy [v,w] (app 'G.basicUnsafeCopy [unwrapMut (var v), unwrap (var w)]),
              fun 'G.elemseq [v,a,b] (app 'G.elemseq [unwrap (var v), unwrapElem (var a), var b])
            ]
    return (InstanceD (cxt ++ unboxCxt tyVars unboxInternals) instanceTy decls)
    
unboxInstance ty tyVars cxt unboxInternals = do
    return (InstanceD (cxt ++ unboxCxt tyVars unboxInternals) (appTy ''U.Unbox [ty]) [])

---- strictboxes:
newtype StrictBox a = StrictBox { unStrictBox :: a }

(<$>) :: Monad m => (a -> b) -> m a -> m b
(<$>) = liftM

--------------- immutable
newtype instance U.Vector (StrictBox a) = V_StrictBox (S.Vector (StrictBox a))

instance G.Vector U.Vector (StrictBox a) where
    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeFreeze (MV_StrictBox v) = V_StrictBox <$> G.basicUnsafeFreeze v
    {-# INLINE basicUnsafeThaw #-}
    basicUnsafeThaw (V_StrictBox v) = MV_StrictBox <$> G.basicUnsafeThaw v
    {-# INLINE basicLength #-}
    basicLength (V_StrictBox v) = G.basicLength v
    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice m n (V_StrictBox v) = V_StrictBox (G.basicUnsafeSlice m n v)
    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM (V_StrictBox v) n = G.basicUnsafeIndexM v n
    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy (MV_StrictBox v) (V_StrictBox w) = G.basicUnsafeCopy v w
    {-# INLINE elemseq #-}
    elemseq (V_StrictBox v) a b = G.elemseq v a b

------------ mutable
newtype instance U.MVector s (StrictBox a) = MV_StrictBox (S.MVector s (StrictBox a))


instance GM.MVector U.MVector (StrictBox a) where
    {-# INLINE basicLength #-}
    basicLength (MV_StrictBox v) = GM.basicLength v
    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice m n (MV_StrictBox v) = MV_StrictBox (GM.basicUnsafeSlice m n v)
    {-# INLINE basicOverlaps #-}
    basicOverlaps (MV_StrictBox v) (MV_StrictBox w) = GM.basicOverlaps v w
    {-# INLINE basicUnsafeNew #-}
    basicUnsafeNew n = MV_StrictBox <$> GM.basicUnsafeNew n
    {-#  INLINE basicUnsafeReplicate #-}
    basicUnsafeReplicate n a = MV_StrictBox <$> GM.basicUnsafeReplicate n a
    {-# INLINE basicUnsafeRead #-}
    basicUnsafeRead (MV_StrictBox v) n = GM.basicUnsafeRead v n
    {-# INLINE basicUnsafeWrite #-}
    basicUnsafeWrite (MV_StrictBox v) n a = GM.basicUnsafeWrite v n a
    {-# INLINE basicClear #-}
    basicClear (MV_StrictBox v) = GM.basicClear v
    {-# INLINE basicSet #-}
    basicSet (MV_StrictBox v) a = GM.basicSet v a
    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy (MV_StrictBox v) (MV_StrictBox w) = GM.basicUnsafeCopy v w
    {-# INLINE basicUnsafeGrow #-}
    basicUnsafeGrow (MV_StrictBox v) n = MV_StrictBox <$> GM.basicUnsafeGrow v n
