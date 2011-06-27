{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, MultiParamTypeClasses, DeriveDataTypeable, FlexibleInstances #-}

-- | Strict version of "Data.Vector". The representation is identical, but all elements of the vector are guaranteed to be (not necessarily deeply) evaluated. Useful for avoiding space leaks.
--
-- Note that strictness is ensured by evaluating elements on insertion. This means that vectors may still contain bottoms, for example when using 'new'.

module Data.Vector.Strict
  (
     Vector, 
     MVector,
  )
 where

import Prelude(Eq, Ord, Show(..), seq, (.), Monad, ($))
import Data.Typeable(Typeable)
import Data.Data(Data)
import Data.Monoid(Monoid)

import Control.Monad(liftM)
import qualified Prelude
import qualified Data.Vector as V
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Generic as G

(<$>) :: Monad m => (a -> b) -> m a -> m b
(<$>) = liftM

--------------- immutable
newtype Vector a = Vector (V.Vector a)
  deriving(Eq, Ord, Monoid, Typeable, Data)

instance Show a => Show (Vector a) where
    show (Vector v) = (Prelude.++ " :: Data.Vector.Strict.Vector") . ("fromList " Prelude.++) . show . V.toList $ v

type instance G.Mutable Vector = MVector

instance G.Vector Vector a where
    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeFreeze (MVector v) = Vector <$> G.basicUnsafeFreeze v
    {-# INLINE basicUnsafeThaw #-}
    basicUnsafeThaw (Vector v) = MVector <$> G.basicUnsafeThaw v
    {-# INLINE basicLength #-}
    basicLength (Vector v) = G.basicLength v
    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice m n (Vector v) = Vector (G.basicUnsafeSlice m n v)
    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM (Vector v) n = G.basicUnsafeIndexM v n
    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy (MVector v) (Vector w) = G.basicUnsafeCopy v w
    {-# INLINE elemseq #-}
    elemseq _v a b = a `seq` b

------------ mutable


newtype MVector s a = MVector (V.MVector s a)
  deriving(Typeable)

instance GM.MVector MVector a where
    {-# INLINE basicLength #-}
    basicLength (MVector v) = GM.basicLength v
    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice m n (MVector v) = MVector (GM.basicUnsafeSlice m n v)
    {-# INLINE basicOverlaps #-}
    basicOverlaps (MVector v) (MVector w) = GM.basicOverlaps v w
    {-# INLINE basicUnsafeNew #-}
    basicUnsafeNew n = MVector <$> GM.basicUnsafeNew n
    {-#  INLINE basicUnsafeReplicate #-}
    basicUnsafeReplicate n a = a `seq` MVector <$> GM.basicUnsafeReplicate n a
    {-# INLINE basicUnsafeRead #-}
    basicUnsafeRead (MVector v) n = GM.basicUnsafeRead v n
    {-# INLINE basicUnsafeWrite #-}
    basicUnsafeWrite (MVector v) n a = a `seq` GM.basicUnsafeWrite v n a
    {-# INLINE basicClear #-}
    basicClear (MVector v) = GM.basicClear v
    {-# INLINE basicSet #-}
    basicSet (MVector v) a = a `seq` GM.basicSet v a
    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy (MVector v) (MVector w) = GM.basicUnsafeCopy v w
    {-# INLINE basicUnsafeGrow #-}
    basicUnsafeGrow (MVector v) n = MVector <$> GM.basicUnsafeGrow v n
