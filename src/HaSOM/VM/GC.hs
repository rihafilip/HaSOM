-- | Object garbage collector definition
module HaSOM.VM.GC
  ( GC,
    empty,
    nil,
    new,
    fromList,
    getAt,
    setAt,
    sweep,
    isToRun,
    statistics,
  )
where

import qualified Data.Bifunctor as Bf
import qualified Data.HashSet as Set
import qualified Data.Stack as St
import Data.Vector ((!?), (//))
import qualified Data.Vector as V
import HaSOM.VM.Object

-- | Representation of garbage collector,
-- parametrised by the native function type
data GC a = MkGC
  { heap :: V.Vector a,
    availableIndices :: St.Stack ObjIx,
    nilObj :: a,
    nilIx :: ObjIx
  }

instance Show (GC a) where
  show MkGC {..} =
    "GC{Indices"
      ++ show availableIndices
      ++ ", heapSize("
      ++ show (V.length heap)
      ++ ")}"

-- | Create a new empty GC with given nil object
empty :: a -> GC a
empty nilObj =
  MkGC
    { heap = V.replicate 20 nilObj, -- Default heap size
      availableIndices = foldr St.push St.emptyStack [1 .. 19],
      nilObj,
      nilIx = 0
    }

-- | Create a new empty GC with given nil object
-- and with objects on given indices.
-- The nilObj is expected to be on index 0
fromList :: a -> [(ObjIx, a)] -> GC a
fromList nilObj objs =
  MkGC
    { heap,
      availableIndices = Set.foldr St.push St.emptyStack indices,
      nilObj,
      nilIx = 0
    }
  where
    maxIx = foldl max 0 $ map fst objs
    -- indicesList =
    indices =
      Set.fromList [1 .. maxIx] `Set.difference` Set.fromList (map fst objs)
    heap =
      V.replicate (getIx maxIx + 1) nilObj // map (Bf.first getIx) objs

-- | Get the index of nil object
nil :: GC a -> ObjIx
nil = nilIx

reserve :: GC a -> GC a
reserve gc@MkGC {..} =
  gc
    { heap = newHeap,
      availableIndices = newIndicesSt
    }
  where
    len = V.length heap
    nextSize = (len `div` 2) + 20
    nextArray = V.replicate nextSize nilObj
    newIndices = [ix len .. ix (len + nextSize - 1)]
    newHeap = (V.++) heap nextArray
    newIndicesSt = foldr St.push availableIndices newIndices

-- | Get a new object index, initializing it to nil
new :: GC a -> (GC a, ObjIx)
new gc@MkGC {..} =
  case St.pop availableIndices of
    Just (newSt, idx) ->
      ( gc
          { heap = heap // [(getIx idx, nilObj)],
            availableIndices = newSt
          },
        idx
      )
    Nothing -> new $ reserve gc

-- | Get the object at given index
getAt :: ObjIx -> GC a -> Maybe a
getAt idx = (!? getIx idx) . heap

-- | Set the object at given index
setAt :: ObjIx -> a -> GC a -> GC a
setAt idx obj gc@MkGC {heap}
  | getIx idx >= V.length heap || idx < 0 = gc
  | otherwise = gc {heap = heap // [(getIx idx, obj)]}

-- | Free unused object, where the given indices are still used
sweep :: Set.HashSet ObjIx -> GC a -> GC a
sweep collected gc@MkGC {heap} =
  gc {availableIndices = newIndices}
  where
    newIndices =
      Set.foldr St.push St.emptyStack $
        Set.fromList [1 .. ix (V.length heap - 1)] `Set.difference` collected

-- | Check if the GC has less than 5% free space, marking that is should be sweeped
--
-- TODO optimize
isToRun :: GC a -> Bool
isToRun MkGC{..} = fromIntegral ( St.size availableIndices )
  < (0.05 :: Double) * fromIntegral (V.length heap)

-- | Return the heap size and available space
statistics :: GC a -> (Int, Int)
statistics MkGC {..} = (V.length heap, St.size availableIndices)
