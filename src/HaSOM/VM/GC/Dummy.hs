-- | Dummy Garbage Collector that doesn't collect
module HaSOM.VM.GC.Dummy (DummyGC, emptyDummyGC) where

import qualified Data.Map.Strict as Map
import HaSOM.VM.GC
import HaSOM.VM.Primitive

-- | The base type of DummyGC
data DummyGC = MkDummyGC
  { objs :: Map.Map ObjIx VMObject,
    nilObj :: VMObject
  }

instance GCClass DummyGC where
  nil = const 0
  new gc@MkDummyGC {objs, nilObj} = (mkGC (gc {objs = newObjs}), newIdx)
    where
      newIdx = Map.foldlWithKey (\a b _ -> a `max` b) 0 objs
      newObjs = Map.insert (newIdx + 1) nilObj objs
  getAt = flip Map.lookup . objs
  setAt gc@MkDummyGC {objs} idx obj = mkGC $ gc {objs = Map.insert idx obj objs}

-- | Get a new Dummy GC with given nil object
emptyDummyGC :: VMObject -> GC
emptyDummyGC nilObj = mkGC $ MkDummyGC (0 `Map.singleton` nilObj) nilObj
