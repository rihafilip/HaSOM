-- | VM Array data type
module HaSOM.VM.Primitive.VMArray
  ( -- * Data type representation
    VMArray,
    newArray,

    -- * Element manipulation
    getArray,
    setArray,

    -- * Array appending functions
    appendArray,
    appendArray',
  )
where

import Combinator ((...))
import HaSOM.VM.Primitive.Ix (ArrayIx)

-- | Type definition of the array
newtype VMArray a = MkVMArray [a]

-- | Create an empty array with given length and default item
newArray :: ArrayIx -> a -> VMArray a
newArray l x = MkVMArray (replicate l x)

-- | Get an element on given array index
getArray :: VMArray a -> ArrayIx -> Maybe a
getArray (MkVMArray xs) idx
  | idx < 0 = Nothing
  | otherwise =
      either (const Nothing) Just $
        foldl go (Left idx) xs
  where
    go :: Either ArrayIx a -> a -> Either ArrayIx a
    go r@(Right _) _ = r
    go (Left 0) x = Right x
    go (Left i) _ = Left (i - 1)

-- | Set an element in given array index
setArray :: VMArray a -> ArrayIx -> a -> Maybe (VMArray a)
setArray (MkVMArray xs) idx element
  | idx > length xs = Nothing
  | otherwise = Just $ MkVMArray $ zipWith f [0 ..] xs
  where
    f i x
      | i == idx = element
      | otherwise = x

-- | Append an element to the back of the array,
-- returning the modified array and the new elements index
appendArray :: VMArray a -> a -> (VMArray a, ArrayIx)
appendArray (MkVMArray xs) element = (MkVMArray (xs ++ [element]), length xs + 1)

-- | Same as appendArray, returning only the new array
appendArray' :: VMArray a -> a -> VMArray a
appendArray' = fst ... appendArray