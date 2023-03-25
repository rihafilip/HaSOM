-- | Collection of all indexing VM types
module HaSOM.VM.Primitive.Ix(ObjIx, InsIx, ClassIx, SymbolIx, ArrayIx, FieldIx) where

-- | Unique object id
type ObjIx = Int

-- | Unique class id
type ClassIx = Int

-- | Unique symbol id
type SymbolIx = Int

----------------------------------

-- | Instruction pointer
type InsIx = Int

-- | Index in Array
type ArrayIx = Int

----------------------------------

-- | Index in field
type FieldIx = Int
