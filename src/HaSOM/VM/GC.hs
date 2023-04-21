-- | Object garbage collector definition
module HaSOM.VM.GC
  ( GC,
    empty,
    nil,
    new,
    getAt,
    setAt,
  )
where

import qualified Data.Map.Strict as Map
import HaSOM.VM.Object

-- TODO as Array
-- | Representation of garbage collector,
-- parametrised by the native function type
data GC a = MkGC
  { heap :: Map.Map ObjIx a,
    nilObj :: a
  }

-- | Create a new empty GC with given nil object
empty :: a -> GC a
empty nilObj = MkGC (0 `Map.singleton` nilObj) nilObj

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
getAt ::  ObjIx -> GC a -> Maybe a
getAt idx = Map.lookup idx . heap

-- | Set the object at given index
setAt ::  ObjIx -> a -> GC a -> GC a
setAt idx obj gc@MkGC {heap} = gc {heap = Map.insert idx obj heap}
