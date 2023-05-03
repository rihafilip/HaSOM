{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Type definitions and helpers for primitive functions
module HaSOM.VM.Primitive.Type
  ( -- * Compiler type
    PrimitiveContainer (..),

    -- * Pure function helper
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
    castString,
    castSymbol,
    castStringOrSymbol,
    castArray,

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
  getArgs :: (UniverseEff r, Lifted IO r) => Eff r (LocalIx, ArgList n ObjIx)

instance GetArgs 'Zero where
  getArgs = pure (1, Nil)

instance GetArgs n => GetArgs ('Succ n) where
  getArgs = do
    (next, xs) <- getArgs @n
    x <- getLocal 0 next
    pure (next + 1, x :+: xs)

-- | Create a function with giving self and arguments
nativeFun :: forall n. (GetArgs n) => (forall r. (UniverseEff r, Lifted IO r) => ObjIx -> ArgList n ObjIx -> Eff r ()) -> NativeFun
nativeFun f = mkNativeFun $ do
  self <- getSelf
  (_, args) <- getArgs @n
  f self args
  void popCallFrame

-- | Create a function with giving self, arguments and returning result on stack
pureNativeFun :: forall n. (GetArgs n) => (forall r. (UniverseEff r, Lifted IO r) => ObjIx -> ArgList n ObjIx -> Eff r ObjIx) -> NativeFun
pureNativeFun f = nativeFun (\self args -> f self args >>= pushStack)

nativeNotImplemented :: NativeFun
nativeNotImplemented = pureNativeFun @N0 $ \self Nil -> do
  lift $ putStrLn "TODO: Not yet implemented"
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
