{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Type definitions and helpers for primitive functions
module HaSOM.VM.Primitive.Type
  ( -- * Compiler type
    PrimitiveContainer (..),

    -- * Pure function helper
    RestrictedNativeFun,
    nativeFun,
    pureNativeFun,

    -- ** Arguments types
    N0,
    N1,
    N2,
    N3,
    ArgList (..),

    -- * Object casting helpers

    -- ** Cast error types
    ObjectType (..),
    formatObjectT,
    fromObject,
    wrongObjectType,

    -- ** Cast an object to type
    castInt,
    castDouble,
    castPromoteDouble,
    castString,
    castSymbol,
    castStringOrSymbol,
    castArray,

    -- * Number methods helper
    numberBoolMethod,

    -- * Not implemented helper
    nativeNotImplemented,
  )
where

import Control.Eff
import Control.Eff.ExcT
import Control.Monad (void)
import Data.Text (Text)
import Data.Text.Utility ((<+))
import HaSOM.VM.Object
import HaSOM.VM.Universe
import HaSOM.VM.Universe.Operations
import qualified HaSOM.VM.VMArray as Arr
import GHC.Float (int2Double)

data PrimitiveContainer = MkPrimitiveContainer
  { name :: Text,
    instanceMethods :: [(Text, NativeFun)],
    classMethods :: [(Text, NativeFun)]
  }

-----------------------------------------------------

-- | Enumeration over object types
data ObjectType
  = InstanceT
  | ClassT
  | IntT
  | DoubleT
  | StringT
  | SymbolT
  | ArrayT
  | BlockT
  | MethodT
  | PrimitiveT
  | NumT
  deriving (Eq, Show)

-- | Object type to String
formatObjectT :: ObjectType -> Text
formatObjectT = \case
  InstanceT -> "Instance"
  ClassT -> "Class"
  IntT -> "Int"
  DoubleT -> "Double"
  StringT -> "String"
  SymbolT -> "Symbol"
  ArrayT -> "Array"
  BlockT -> "Block"
  MethodT -> "Method"
  PrimitiveT -> "Primitive"
  NumT -> "Int or Double"

-- | Get object enumeration based on it's type
fromObject :: VMObjectNat -> ObjectType
fromObject = \case
  InstanceObject {} -> InstanceT
  ClassObject {} -> ClassT
  IntObject {} -> IntT
  DoubleObject {} -> DoubleT
  StringObject {} -> StringT
  SymbolObject {} -> SymbolT
  ArrayObject {} -> ArrayT
  BlockObject {} -> BlockT
  MethodObject {} -> MethodT
  PrimitiveObject {} -> PrimitiveT

-- | Throw an error message of wrong object type
wrongObjectType :: (Member ExcT r) => VMObjectNat -> ObjectType -> Eff r a
wrongObjectType obj expected =
  throwT $
    "Incorrect object type: Expected "
      <+ formatObjectT expected
      <+ ", but instead got "
      <+ formatObjectT (fromObject obj)

-----------------------------------------------------

data Nat = Zero | Succ Nat

-- | Zero arguments type
type N0 = 'Zero

-- | One arguments type
type N1 = 'Succ 'Zero

-- | Two arguments type
type N2 = 'Succ N1

-- | Three arguments type
type N3 = 'Succ N2

-----------------------------------------------------
infixr 4 :+:

-- | List with fixed size
data ArgList (size :: Nat) a where
  Nil :: ArgList 'Zero a
  (:+:) :: a -> ArgList n a -> ArgList ('Succ n) a

class GetArgs (n :: Nat) where
  -- | Get arguments list of size given by the type
  getArgs :: (UniverseEff r, Lifted IO r) => LocalIx -> Eff r (ArgList n ObjIx)

instance GetArgs 'Zero where
  getArgs _ = pure Nil

instance GetArgs n => GetArgs ('Succ n) where
  getArgs n = do
    x <- getLocal 0 n
    xs <- getArgs @n (n+1)
    pure (x :+: xs)

type RestrictedNativeFun r = (Lifted IO r, [
    State VMGlobalsNat,
    Reader CoreClasses,
    State VMLiterals,
    State GCNat,
    State GCFlag,
    Reader RuntimeStartTime,
    ExcT
  ] <:: r)

-- | Create a function with giving self and arguments
nativeFun :: forall n. (GetArgs n) => (forall r. RestrictedNativeFun r => ObjIx -> ArgList n ObjIx -> Eff r ()) -> NativeFun
nativeFun f = mkNativeFun $ do
  self <- getSelf
  args <- getArgs @n 1
  f self args
  void popCallFrame
  pure Nothing

-- | Create a function with giving self, arguments and returning result on stack
pureNativeFun :: forall n. (GetArgs n) => (forall r. RestrictedNativeFun r => ObjIx -> ArgList n ObjIx -> Eff r ObjIx) -> NativeFun
pureNativeFun f = mkNativeFun $ do
  self <- getSelf
  args <- getArgs @n 1
  f self args >>= pushStack
  void popCallFrame
  pure Nothing

nativeNotImplemented :: String -> NativeFun
nativeNotImplemented str = pureNativeFun @N0 $ \self Nil -> do
  lift $ putStrLn $ "TODO: Not yet implemented '" ++ str ++ "'"
  pure self

--------------------------------------------------

castInt :: (GCEff r, Member ExcT r) => ObjIx -> Eff r Int
castInt idx =
  getAsObject idx >>= \case
    IntObject {intValue} -> pure intValue
    obj -> wrongObjectType obj IntT

castDouble :: (GCEff r, Member ExcT r) => ObjIx -> Eff r Double
castDouble idx =
  getAsObject idx >>= \case
    DoubleObject {doubleValue} -> pure doubleValue
    obj -> wrongObjectType obj DoubleT

castPromoteDouble :: (GCEff r, Member ExcT r) => ObjIx -> Eff r Double
castPromoteDouble idx =
  getAsObject idx >>= \case
    IntObject {intValue} -> pure (int2Double intValue)
    DoubleObject {doubleValue} -> pure doubleValue
    obj -> wrongObjectType obj NumT

castString :: (GCEff r, Member ExcT r) => ObjIx -> Eff r Text
castString idx =
  getAsObject idx >>= \case
    StringObject {stringValue} -> pure stringValue
    obj -> wrongObjectType obj StringT

castSymbol :: (GCEff r, Member ExcT r) => ObjIx -> Eff r Text
castSymbol idx =
  getAsObject idx >>= \case
    SymbolObject {symbolValue} -> pure symbolValue
    obj -> wrongObjectType obj SymbolT

castStringOrSymbol :: (GCEff r, Member ExcT r) => ObjIx -> Eff r Text
castStringOrSymbol idx =
  getAsObject idx >>= \case
    StringObject {stringValue} -> pure stringValue
    SymbolObject {symbolValue} -> pure symbolValue
    obj -> wrongObjectType obj SymbolT

castArray :: (GCEff r, Member ExcT r) => ObjIx -> Eff r (Arr.VMArray FieldIx ObjIx, Arr.VMArray FieldIx ObjIx -> VMObjectNat)
castArray idx =
  getAsObject idx >>= \case
    obj@ArrayObject {arrayValue} -> pure (arrayValue, \val -> obj {arrayValue = val})
    obj -> wrongObjectType obj ArrayT

--------------------------------------------------

numberBoolMethod :: (forall a . (Eq a, Ord a) => a -> a -> Bool) -> NativeFun
numberBoolMethod op = pureNativeFun @N1 $ \self (other :+: Nil) -> do
  selfObj <- getAsObject self
  otherObj <- getAsObject other

  let res = case (selfObj, otherObj) of
        (IntObject {intValue = i1}, IntObject {intValue = i2}) ->
          i1 `op` i2
        (IntObject {intValue = i1}, DoubleObject {doubleValue = d2}) ->
          int2Double i1 `op` d2
        (DoubleObject {doubleValue = d1}, IntObject {intValue = i2}) ->
          d1 `op` int2Double i2
        (DoubleObject {doubleValue = d1}, DoubleObject {doubleValue = d2}) ->
          d1 `op` d2
        _ -> False

  if res
    then newTrue >>= addToGC
    else newFalse >>= addToGC
