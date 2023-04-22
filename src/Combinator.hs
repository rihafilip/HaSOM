-- | Collection of helper combinators
module Combinator((...), (.>)) where

-- | Blackbird combinator
(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)

infixl 9 .>

(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)
