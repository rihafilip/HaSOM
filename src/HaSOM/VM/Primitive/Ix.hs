-- | Collection of all indexing VM types
module HaSOM.VM.Primitive.Ix(ObjIx, InsIx, ClassIx, SymbolIx, ArrayIx) where

-- | Unique object id
type ObjIx = Int

-- | Instruction pointer indexer
type InsIx = Int

-- | Unique class id
type ClassIx = Int

-- | Unique symbol id
type SymbolIx = Int

-- | Array indexer
type ArrayIx = Int
