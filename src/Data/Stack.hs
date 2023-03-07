-- | Stack data type
module Data.Stack
  ( -- * Stack definition
    Stack,
    StackIx,
    emptyStack,

    -- * Traditional stack operations
    push,
    pop,
    popSt,
    popn,
    top,

    -- * Indexed stack operations
    reserve,
    getAt,
  )
where

import Data.Maybe.Utility (maybeFromRight)

-- | Index in stack
type StackIx = Int

-- | The type of indexed stack
newtype Stack a = MkStack
  { stackData :: [a]
  }

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
  | otherwise = Just st {stackData = take n stackData}

-- | Same as pop, returning only the top item
top :: Stack a -> Maybe a
top = fmap snd . pop

---------------------------------------

-- | Push the given item n times on top of the stack
reserve :: StackIx -> a -> Stack a -> Stack a
reserve n x st@MkStack {stackData} = st {stackData = replicate n x ++ stackData}

-- | Get the item on given index, indexing from bottom
getAt :: StackIx -> Stack a -> Maybe a
getAt idx MkStack {stackData} =
  maybeFromRight $ foldr f (Left idx) stackData
  where
    f :: a -> Either Int a -> Either Int a
    f x (Left 0) = Right x
    f _ (Left x) = Left (x - 1)
    f _ r@(Right _) = r