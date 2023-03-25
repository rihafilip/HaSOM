-- | Helper functions on Maybe data type
module Data.Maybe.Utility(maybeFromLeft, maybeFromRight) where

-- | Create a Maybe from Either on left
maybeFromLeft :: Either a b -> Maybe a
maybeFromLeft = either Just (const Nothing)

-- | Create a Maybe from Either on right
maybeFromRight :: Either a b -> Maybe b
maybeFromRight = either (const Nothing) Just
