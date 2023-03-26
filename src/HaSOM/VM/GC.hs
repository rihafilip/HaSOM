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
data GC a = MkGC
  { heap :: Map.Map ObjIx a,
    nilObj :: a
  }

-- | Create a new empty GC with given nil object
emptyGC :: a -> GC a
emptyGC nilObj = MkGC (0 `Map.singleton` nilObj) nilObj

-- | Get the index of nil object
nil :: GC a -> ObjIx
nil = const 0

-- | Get a new object index, initializing it to nil
new :: GC a -> (GC a, ObjIx)
new gc@MkGC {heap, nilObj} = (gc {heap = newheap}, newIdx)
  where
    newIdx = (+1) $ Map.foldlWithKey (\a b _ -> a `max` b) 0 heap
    newheap = Map.insert newIdx nilObj heap

-- | Get the object at given index
getAt :: GC a -> ObjIx -> Maybe a
getAt = flip Map.lookup . heap

-- | Set the object at given index
setAt :: GC a -> ObjIx -> a -> GC a
setAt gc@MkGC {heap} idx obj = gc {heap = Map.insert idx obj heap}
