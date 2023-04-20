module HaSOM.Compiler.LookupMap where

import qualified Data.HashMap.Strict as Map
import Data.Hashable (Hashable)
import HaSOM.VM.Object.Ix (VMIx (..))
import Combinator ((...))

data LookupMap i v = MkLookupMap
  { lookups :: Map.HashMap i v,
    nextIx :: v
  }

newLM :: (VMIx v) => LookupMap i v
newLM = MkLookupMap Map.empty (ix 0)

fromListLM :: (VMIx v, Eq i, Hashable i, Enum v) => [i] -> LookupMap i v
fromListLM = putAllLM newLM

getLM :: (Eq i, Hashable i) => LookupMap i v -> i -> Maybe v
getLM (MkLookupMap {lookups}) key = Map.lookup key lookups

getOrSetLM :: (Eq i, Hashable i, Enum v) => LookupMap i v -> i -> (LookupMap i v, v)
getOrSetLM m@(MkLookupMap {lookups, nextIx}) key = case Map.lookup key lookups of
  Nothing -> (MkLookupMap (Map.insert key nextIx lookups) (succ nextIx), nextIx)
  Just v -> (m, v)

putAllLM :: (Eq i, Hashable i, Enum v) => LookupMap i v -> [i] -> LookupMap i v
putAllLM = foldl f
  where
    f = fst ... getOrSetLM
