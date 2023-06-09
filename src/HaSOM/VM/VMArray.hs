{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | VM Array data type
module HaSOM.VM.VMArray
  ( -- * Data type representation
    VMArray,

    -- * Construction
    new,
    fromList,

    -- * Element manipulation
    get,
    set,

    -- * Sized function
    length,
    elemIndex,
    append,

    -- * Transformation
    toList,
  )
where

import HaSOM.VM.Object.Ix (VMIx (..))
import Prelude hiding (length)
import Data.Vector (Vector, (!?), (//))
import qualified Data.Vector as V

-- | Type definition of the array,
-- where i is the index type and a is the element type
newtype VMArray i a = MkVMArray (Vector a)
  deriving newtype (Functor, Applicative, Monad, Foldable, Eq, Show)

instance Traversable (VMArray i) where
  traverse f (MkVMArray xs) = MkVMArray <$> traverse f xs

----------------------------------------------------------

-- | Create an empty array with given length and default item
new :: Int -> a -> VMArray i a
new l x = MkVMArray (V.replicate l x)

-- | Create an empty array from a list
fromList :: [a] -> VMArray i a
fromList = MkVMArray . V.fromList

----------------------------------------------------------

-- | Get an element on given array index
get :: VMIx i => i -> VMArray i a -> Maybe a
get i (MkVMArray xs) = xs !? getIx i

-- | Set an element in given array index
set :: VMIx i => i -> a -> VMArray i a -> Maybe (VMArray i a)
set idx element (MkVMArray xs)
  | i < 0 || i >= V.length xs = Nothing
  | otherwise = Just $ MkVMArray $ xs // [(i, element)]
  where i = getIx idx

----------------------------------------------------------

length :: VMArray i a -> Int
length (MkVMArray xs) = V.length xs

elemIndex :: (VMIx i, Eq a) => a -> VMArray i a -> Maybe i
elemIndex el (MkVMArray xs) = ix <$> V.elemIndex el xs

append :: VMArray i a -> VMArray i a -> VMArray i a
append (MkVMArray xs) (MkVMArray ys) = MkVMArray $ (V.++) xs ys

----------------------------------------------------------
-- | Transform array to list
toList :: VMArray i a -> [a]
toList (MkVMArray a) = V.toList a
