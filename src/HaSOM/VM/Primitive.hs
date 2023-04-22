{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

-- | Primitive compilation and default primitives definition
module HaSOM.VM.Primitive (defaultPrimitives, compilePrimitives) where

import Control.Eff (Eff, Member)
import Control.Eff.State.Strict
import qualified Data.HashMap.Strict as Map
import Data.Text (Text)
import Data.Text.Utility ((<+))
import HaSOM.Compiler.Context (GlobalCtx (..), getLiteral)
import HaSOM.VM.Object (VMLiteral (SymbolLiteral))
import qualified HaSOM.VM.Primitive.Array as Array
import qualified HaSOM.VM.Primitive.Block as Block
import qualified HaSOM.VM.Primitive.Block1 as Block1
import qualified HaSOM.VM.Primitive.Block2 as Block2
import qualified HaSOM.VM.Primitive.Block3 as Block3
import qualified HaSOM.VM.Primitive.Class as Class
import qualified HaSOM.VM.Primitive.Double as Double
import qualified HaSOM.VM.Primitive.Integer as Integer
import qualified HaSOM.VM.Primitive.Method as Method
import qualified HaSOM.VM.Primitive.Object as Object
import qualified HaSOM.VM.Primitive.Primitive as Primitive
import qualified HaSOM.VM.Primitive.String as String
import qualified HaSOM.VM.Primitive.Symbol as Symbol
import qualified HaSOM.VM.Primitive.System as System
import HaSOM.VM.Primitive.Type
import HaSOM.VM.Universe (NativeFun)

defaultPrimitives :: [PrimitiveContainer]
defaultPrimitives =
  [ Array.primitives,
    Block.primitives,
    Block1.primitives,
    Block2.primitives,
    Block3.primitives,
    Class.primitives,
    Double.primitives,
    Integer.primitives,
    Method.primitives,
    Object.primitives,
    Primitive.primitives,
    String.primitives,
    Symbol.primitives,
    System.primitives
  ]

compilePrimitives :: (Member (State GlobalCtx) r) => [PrimitiveContainer] -> Eff r ()
compilePrimitives =
  mapM_
    ( \MkPrimitiveContainer {name, instanceMethods, classMethods} -> do
        compilePrimitiveClass name instanceMethods
        compilePrimitiveClass (name <+ " class") classMethods
    )

compilePrimitiveClass :: (Member (State GlobalCtx) r) => Text -> [(Text, NativeFun)] -> Eff r ()
compilePrimitiveClass name ns = do
  mapM_ f ns
  _ <- getLiteral (SymbolLiteral name)
  pure ()
  where
    f :: (Member (State GlobalCtx) r) => (Text, NativeFun) -> Eff r ()
    f (mName, m) = do
      ctx@MkGlobalCtx {primitives} <- get @GlobalCtx
      let newPrimitives = Map.insert (name, mName) m primitives
      put $ ctx {primitives = newPrimitives}
      _ <- getLiteral (SymbolLiteral mName)
      pure ()
