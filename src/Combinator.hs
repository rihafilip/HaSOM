module Combinator where

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)
