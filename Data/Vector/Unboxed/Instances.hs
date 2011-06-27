{-# LANGUAGE TemplateHaskell, TypeFamilies, MultiParamTypeClasses, ScopedTypeVariables #-}

-- | Some useful 'Unbox' instances: 'Data.Text.Text', 'Data.ByteString.ByteString'
module Data.Vector.Unboxed.Instances(Unbox, Vector) where

import Data.Vector.Unboxed
import Data.Vector.Unboxed.Derive

import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.ByteString
import qualified Data.ByteString.Lazy

import Data.Binary
import qualified Data.Vector.Generic as G
import Control.Monad

import System.IO.Unsafe
import qualified Data.Vector.Generic.Mutable as M

deriveMany 
   [
     ''Data.Text.Text :~ StrictBoxed
   , ''Data.Text.Lazy.Text :~ StrictBoxed
   , ''Data.ByteString.ByteString :~ StrictBoxed
   , ''Data.ByteString.Lazy.ByteString :~ StrictBoxed
   , ''Data.Vector.Unboxed.Vector :~ StrictBoxed
   , ''Data.Vector.Unboxed.MVector :~ StrictBoxed
   ]
   
instance (Unbox a, Binary a) => Binary (Vector a) where
    put v = do
        put (G.length v)
        G.mapM_ put v

    -- this is morally sound, if very awkward.
    -- all effects are contained, and can't escape the unsafeFreeze
    {-# INLINE get #-}
    get = do
        n  <- get

        -- new unitinialized array
        mv <- lift $ M.new n

        let fill i
                | i < n = do
                    x <- get
                    (unsafePerformIO $ M.unsafeWrite mv i x) `seq` return ()
                    fill (i+1)

                | otherwise = return ()

        fill 0

        lift $ G.unsafeFreeze mv

lift = return .unsafePerformIO

{-
instance Unbox a => Suitable Vector a where
    constraints = Unboxable

data instance Constraints Vector a where
    Unboxable :: Unbox a => Constraints Vector a

-- | 'fmap' for vectors
instance RFunctor Vector where
    fmap (f :: a -> b) = case (constraints :: Constraints Vector a, constraints :: Constraints Vector b) of
        (Unboxable, Unboxable) -> Data.Vector.Unboxed.map
-}