{-# LANGUAGE InstanceSigs #-}
-- | Interning helper collection.
--
-- It exposes an interface for giving a new index continuously
-- for each new given key
module Data.LookupMap
  ( -- * Data type defintiion
    LookupMap,

    -- * Construction
    new,
    fromList,

    -- * Element manipulation
    get,
    getOrSet,

    -- * Batch manipulation
    putAll,

    -- * Extraction
    toList,
    toHashMap,
  )
where

import Combinator ((...))
import qualified Data.HashMap.Strict as Map
import Data.Hashable (Hashable)
import HaSOM.VM.Object.Ix (VMIx (..))

-- | LookupMap type definition, where i is the key type and v index type
data LookupMap i v = MkLookupMap
  { lookups :: Map.HashMap i v,
    nextIx :: v
  }

instance (Show i, Show v) => Show (LookupMap i v) where
  show = show . lookups

-- | Create a new empty LookupMap
new :: (VMIx v) => LookupMap i v
new = MkLookupMap Map.empty (ix 0)

-- | Create a new LookupMap from a List,
-- where the given keys are given indexes starting at 0
fromList :: (VMIx v, Eq i, Hashable i, Enum v) => [i] -> LookupMap i v
fromList = (`putAll` new)

-- | Get an index for given key
get :: (Eq i, Hashable i) => i -> LookupMap i v -> Maybe v
get key (MkLookupMap {lookups}) = Map.lookup key lookups

-- | Get an already defined index for given key,
-- or a new one if it is not defined
getOrSet :: (Eq i, Hashable i, Enum v) => i -> LookupMap i v -> (LookupMap i v, v)
getOrSet key m@(MkLookupMap {lookups, nextIx}) = case Map.lookup key lookups of
  Nothing -> (MkLookupMap (Map.insert key nextIx lookups) (succ nextIx), nextIx)
  Just v -> (m, v)

-- | Put all of the given values in the map, giving new indices to th evalues
putAll :: (Eq i, Hashable i, Enum v) => [i] -> LookupMap i v -> LookupMap i v
putAll = flip (foldl f)
  where
    f = fst ... flip getOrSet

-- | Transform the map to list
toList :: LookupMap i v -> [(i, v)]
toList = Map.toList . lookups

-- | Extract the underlying HashMap
toHashMap :: LookupMap i v -> Map.HashMap i v
toHashMap = lookups
