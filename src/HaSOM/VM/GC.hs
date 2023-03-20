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

data GC = MkGC
  { objs :: Map.Map ObjIx VMObject,
    nilObj :: VMObject
  }

emptyGC :: VMObject -> GC
emptyGC nilObj = MkGC (0 `Map.singleton` nilObj) nilObj

nil :: GC -> ObjIx
nil = const 0

new :: GC -> (GC, ObjIx)
new gc@MkGC {objs, nilObj} = (gc {objs = newObjs}, newIdx)
  where
    newIdx = Map.foldlWithKey (\a b _ -> a `max` b) 0 objs
    newObjs = Map.insert (newIdx + 1) nilObj objs

getAt :: GC -> ObjIx -> Maybe VMObject
getAt = flip Map.lookup . objs

setAt :: GC -> ObjIx -> VMObject -> GC
setAt gc@MkGC {objs} idx obj = gc {objs = Map.insert idx obj objs}
