-- | Helper functions on Maybe data type
module Data.Maybe.Utility where

maybeFromLeft :: Either a b -> Maybe a
maybeFromLeft = either Just (const Nothing)

maybeFromRight :: Either a b -> Maybe b
maybeFromRight = either (const Nothing) Just
