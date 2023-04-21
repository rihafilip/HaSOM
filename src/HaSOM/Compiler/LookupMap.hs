module HaSOM.Compiler.LookupMap(LookupMap, new, fromList, get, getOrSet, putAll) where

import qualified Data.HashMap.Strict as Map
import Data.Hashable (Hashable)
import HaSOM.VM.Object.Ix (VMIx (..))
import Combinator ((...))

data LookupMap i v = MkLookupMap
  { lookups :: Map.HashMap i v,
    nextIx :: v
  }

new :: (VMIx v) => LookupMap i v
new = MkLookupMap Map.empty (ix 0)

fromList :: (VMIx v, Eq i, Hashable i, Enum v) => [i] -> LookupMap i v
fromList = (`putAll` new)

get :: (Eq i, Hashable i) =>  i -> LookupMap i v -> Maybe v
get key  (MkLookupMap {lookups}) = Map.lookup key lookups

getOrSet :: (Eq i, Hashable i, Enum v) =>  i -> LookupMap i v -> (LookupMap i v, v)
getOrSet key m@(MkLookupMap {lookups, nextIx}) = case Map.lookup key lookups of
  Nothing -> (MkLookupMap (Map.insert key nextIx lookups) (succ nextIx), nextIx)
  Just v -> (m, v)

putAll :: (Eq i, Hashable i, Enum v) => [i] -> LookupMap i v -> LookupMap i v
putAll = flip (foldr f)
  where
    f = fst ... getOrSet
