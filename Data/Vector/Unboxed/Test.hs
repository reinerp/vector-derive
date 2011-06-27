{-# LANGUAGE TypeFamilies, TemplateHaskell, FlexibleInstances, MultiParamTypeClasses #-}

module Data.Vector.Unboxed.Test(MyInt) where

import Data.Vector.Unboxed.Derive
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM

--data Pair a b = Pair a b
newtype MyInt = MI { unMI :: Int }

{-
instance SimpleRepr MyInt where
    type ElemRep MyInt = Int
    {-# INLINE toRep #-}
    toRep = unMI
    {-# INLINE fromRep #-}
    fromRep = MI
-}

data Orientation = Left | Middle | Right
  deriving(Enum, Show)
deriveOne ''Orientation Enum

deriveOne ''MyInt StrictBoxed
{-
newtype instance U.Vector MyInt = V_Pair { unV_Pair :: U.Vector Int }
newtype instance U.MVector s MyInt = MV_Pair { unMV_Pair :: U.MVector s Int }

instance Repr MyInt where
    type VectorRep MyInt = U.Vector
    {-# INLINE wrapR #-}
    wrapR = V_Pair
    {-# INLINE unwrapR #-}
    unwrapR = unV_Pair
    {-# INLINE mwrapR #-}
    mwrapR = MV_Pair
    {-# INLINE munwrapR #-}
    munwrapR = unMV_Pair
-}

{-
instance GM.MVector (MVectorRep MyInt) MyInt where
    basicLength v = GM.basicLength (mvectorFromRep v)
instance GM.MVector MVector MyInt
-}