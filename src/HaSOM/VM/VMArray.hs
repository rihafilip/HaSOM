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

    -- * Array appending functions
    appendIx,
    append,
  )
where

import Combinator ((...))
import HaSOM.VM.Object.Ix (VMIx (getIx, ix))

-- TODO as Array
-- | Type definition of the array,
-- where i is the index type and a is the element type
newtype VMArray i a = MkVMArray [a]

instance Show a => Show (VMArray i a) where
  show (MkVMArray xs) = show xs

instance Eq a => Eq (VMArray i a) where
  (MkVMArray xs) == (MkVMArray ys) = xs == ys

----------------------------------------------------------

-- | Create an empty array with given length and default item
new :: Int -> a -> VMArray i a
new l x = MkVMArray (replicate l x)

-- | Create an empty array from a list
fromList :: [a] -> VMArray i a
fromList = MkVMArray

----------------------------------------------------------

-- | Get an element on given array index
get :: VMIx i => i -> VMArray i a -> Maybe a
get idx (MkVMArray xs)
  | getIx idx < 0 = Nothing
  | otherwise =
      either (const Nothing) Just $
        foldl go (Left (getIx idx)) xs
  where
    go r@(Right _) _ = r
    go (Left 0) x = Right x
    go (Left i) _ = Left (i - 1)

-- | Set an element in given array index
set :: VMIx i => i -> a -> VMArray i a -> Maybe (VMArray i a)
set idx element (MkVMArray xs)
  | getIx idx < 0 || getIx idx > length xs = Nothing
  | otherwise = Just $ MkVMArray $ zipWith f [0 ..] xs
  where
    f i x
      | i == getIx idx = element
      | otherwise = x

----------------------------------------------------------

-- | Append an element to the back of the array,
-- returning the modified array and the new elements index
appendIx :: VMIx i => a -> VMArray i a -> (VMArray i a, i)
appendIx element (MkVMArray xs) = (MkVMArray (xs ++ [element]), ix (length xs + 1))

-- | Same as appendArrayIx, returning only the new array
append :: VMIx i => a -> VMArray i a -> VMArray i a
append = fst ... appendIx
