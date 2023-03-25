module HaSOM.VM.GC
  ( GC,
    emptyGC,
    nil,
    new,
    getAt,
    setAt,
  )
where

import qualified Data.Map.Strict as Map
import HaSOM.VM.Primitive

-- TODO as Array
-- | Representation of garbage collector,
-- parametrised by the native function type
data GC f = MkGC
  { heap :: Map.Map ObjIx (VMObject f),
    nilObj :: VMObject f
  }

-- | Create a new empty GC with given nil object
emptyGC :: VMObject f -> GC f
emptyGC nilObj = MkGC (0 `Map.singleton` nilObj) nilObj

-- | Get the index of nil object
nil :: GC f -> ObjIx
nil = const 0

-- | Get a new object index, initializing it to nil
new :: GC f -> (GC f, ObjIx)
new gc@MkGC {heap, nilObj} = (gc {heap = newheap}, newIdx)
  where
    newIdx = Map.foldlWithKey (\a b _ -> a `max` b) 0 heap
    newheap = Map.insert (newIdx + 1) nilObj heap

-- | Get the object at given index
getAt :: GC f -> ObjIx -> Maybe (VMObject f)
getAt = flip Map.lookup . heap

-- | Set the object at given index
setAt :: GC f -> ObjIx -> VMObject f -> GC f
setAt gc@MkGC {heap} idx obj = gc {heap = Map.insert idx obj heap}
