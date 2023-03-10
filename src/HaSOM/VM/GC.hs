{-# LANGUAGE GADTs #-}

-- | Polymorphic Garbage Collector definition
module HaSOM.VM.GC
  ( GCClass (..),
    GC,
    mkGC,
  )
where

import HaSOM.VM.Primitive

-- | The base class used for defining GC
class GCClass gc where
  -- | Get a nil object
  nil :: gc -> ObjIx

  -- | Create a new object, populate it with nil object
  -- and return it's index
  new :: gc -> (GC, ObjIx)

  -- | Get the object at given index
  getAt :: gc -> ObjIx -> Maybe VMObject

  -- | Set an object at given index
  setAt :: gc -> ObjIx -> VMObject -> GC

-- | The polymorphic garbage collector type
data GC = forall gc. GCClass gc => MkGC gc

-- | Wrap a garbage collector in polymorphic class
mkGC :: GCClass gc => gc -> GC
mkGC = MkGC

instance GCClass GC where
  nil (MkGC gc) = nil gc
  new (MkGC gc) = new gc
  getAt (MkGC gc) = getAt gc
  setAt (MkGC gc) = setAt gc
