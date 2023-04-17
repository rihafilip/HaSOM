-- | VM Array data type
module HaSOM.VM.Primitive.VMArray
  ( -- * Data type representation
    VMArray,

    -- * Construction
    newArray,
    fromListArray,

    -- * Element manipulation
    getArray,
    setArray,

    -- * Array appending functions
    appendArrayIx,
    appendArray,
  )
where

import Combinator ((...))
import HaSOM.VM.Primitive.Ix (VMIx (getIx, ix))

-- TODO as Array
-- | Type definition of the array,
-- where i is the index type and a is the element type
newtype VMArray i a  = MkVMArray [a]

instance Show a => Show (VMArray i a) where
  show (MkVMArray xs) = show xs

instance Eq a => Eq (VMArray i a) where
  (MkVMArray xs) == (MkVMArray ys) = xs == ys

----------------------------------------------------------

-- | Create an empty array with given length and default item
newArray :: Int -> a -> VMArray i a
newArray l x = MkVMArray (replicate l x)

-- | Create an empty array from a list
fromListArray :: [a] -> VMArray i a
fromListArray = MkVMArray

----------------------------------------------------------

-- | Get an element on given array index
getArray :: VMIx i => VMArray i a -> i -> Maybe a
getArray (MkVMArray xs) idx
  | getIx idx < 0 = Nothing
  | otherwise =
      either (const Nothing) Just $
        foldl go (Left (getIx idx)) xs
  where
    go r@(Right _) _ = r
    go (Left 0) x = Right x
    go (Left i) _ = Left (i - 1)

-- | Set an element in given array index
setArray :: VMIx i => VMArray i a -> i -> a -> Maybe (VMArray i a)
setArray (MkVMArray xs) idx element
  | getIx idx < 0 || getIx idx > length xs = Nothing
  | otherwise = Just $ MkVMArray $ zipWith f [0 ..] xs
  where
    f i x
      | i == getIx idx = element
      | otherwise = x

----------------------------------------------------------

-- | Append an element to the back of the array,
-- returning the modified array and the new elements index
appendArrayIx :: VMIx i => VMArray i a -> a -> (VMArray i a, i)
appendArrayIx (MkVMArray xs) element = (MkVMArray (xs ++ [element]), ix (length xs + 1) )

-- | Same as appendArrayIx, returning only the new array
appendArray :: VMIx i => VMArray i a -> a -> VMArray i a
appendArray = fst ... appendArrayIx
