-- | Stack data type
module Data.Stack
  ( -- * Data type definition
    Stack,
    StackIx,

    -- * Construction
    emptyStack,

    -- * Traditional stack operations
    push,
    pop,
    popSt,
    popn,
    top,

    -- * Size of stack
    size,
  )
where

-- | Index in stack
type StackIx = Int

-- | The type of indexed stack
newtype Stack a = MkStack
  { stackData :: [a]
  }

---------------------------------------

instance Show a => Show (Stack a) where
  show = show . stackData

instance Eq a => Eq (Stack a) where
  (MkStack x) == (MkStack y) = x == y

---------------------------------------

-- | Create an empty stack
emptyStack :: Stack a
emptyStack = MkStack []

---------------------------------------

-- | Push an item to the top of the stack
push :: a -> Stack a -> Stack a
push el st@MkStack {stackData} = st {stackData = el : stackData}

-- | Pop the top item from stack, returning nothing if stack is empty,
-- otherwise returning the top item and the stack
pop :: Stack a -> Maybe (Stack a, a)
pop MkStack {stackData = []} = Nothing
pop st@MkStack {stackData = x : xs} = Just (st {stackData = xs}, x)

-- | Same as pop, returning only the stack
popSt :: Stack a -> Maybe (Stack a)
popSt = fmap fst . pop

-- | Same as pop repeated n times
popn :: StackIx -> Stack a -> Maybe (Stack a)
popn n st@MkStack {stackData}
  | n > length stackData = Nothing
  | otherwise = Just st {stackData = drop n stackData}

-- | Same as pop, returning only the top item
top :: Stack a -> Maybe a
top = fmap snd . pop

---------------------------------------

-- | Return the size of the stack
size :: Stack a -> Int
size = length . stackData
