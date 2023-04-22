{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | Compiler context definition and helpers
module HaSOM.Compiler.Context
  ( -- * Contexts definition
    GlobalCtx (..),
    ClassCtx (..),
    BlockCtx (..),

    -- ** Fields lookup map alias
    FieldsLookup,

    -- * Effects definition
    ClassEff,
    MethodEff,
    BlockEff,
    ExprEff,

    -- * Context helpers
    methodCtx,
    nestedBlockCtx,
    isBlock,
    getLiteral,

    -- * Variable lookup helpers
    VarRes(..),
    findVar,
    findGlobalVar,
    findClassVar,
    findBlockVar,
  )
where

import Control.Applicative ((<|>))
import Control.Eff
import Control.Eff.Reader.Strict
import Control.Eff.State.Strict
import qualified Data.Bifunctor as Bf
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as Map
import Data.Text (Text)
import HaSOM.Compiler.LookupMap (LookupMap)
import qualified HaSOM.Compiler.LookupMap as LM
import HaSOM.VM.Object hiding (getLiteral)
import HaSOM.VM.Universe (NativeFun)

------------------------------------------------------------
-- Compilation contexts

data GlobalCtx = MkGlobalCtx
  { globals :: LookupMap Text GlobalIx,
    symbols :: LookupMap VMLiteral LiteralIx,
    primitives :: Map.HashMap (Text, Text) NativeFun
  }

type FieldsLookup = Map.HashMap Text FieldIx

data ClassCtx = MkClassCtx
  { className :: Text,
    fields :: FieldsLookup
  }

data BlockCtx
  = MethodCtx
      { locals :: LookupMap Text LocalIx
      }
  | NestedBlockCtx
      { previous :: BlockCtx,
        locals :: LookupMap Text LocalIx
      }

------------------------------------------------------------
-- Effects

type ClassEff r = Member (State GlobalCtx) r

type MethodEff r = [Reader ClassCtx, State GlobalCtx] <:: r

type BlockEff r = [Reader ClassCtx, Reader BlockCtx, State GlobalCtx] <:: r

type ExprEff r = [Reader ClassCtx, Reader BlockCtx, State GlobalCtx] <:: r

------------------------------------------------------------
-- Context helpers

-- | Create method context
methodCtx :: [Text] -> BlockCtx
methodCtx = MethodCtx . LM.fromList . ("self" :)

-- | Create a nested block context
nestedBlockCtx :: [Text] -> BlockCtx -> BlockCtx
nestedBlockCtx = flip NestedBlockCtx . LM.fromList . ("self" :)

-- | Ask if current environment is in block
isBlock :: Member (Reader BlockCtx) r => Eff r Bool
isBlock =
  ask <&> \case
    MethodCtx {} -> False
    NestedBlockCtx {} -> True

------------------------------------------------------------
-- Literal resolving

-- | Resolve literal, adding it to context if it is not there already
getLiteral :: Member (State GlobalCtx) r => VMLiteral -> Eff r LiteralIx
getLiteral sym = do
  ctx@MkGlobalCtx {symbols} <- get
  let (newSym, symIx) = LM.getOrSet sym symbols
  put (ctx {symbols = newSym})
  pure symIx

------------------------------------------------------------
-- Variable resolving

-- | Varibale lookup result
data VarRes
  = GlobalVar GlobalIx
  | FieldVar FieldIx
  | SuperVar FieldIx
  | LocalVar LocalIx LocalIx -- EnvironmentIx, Local

-- | Find variable
findVar :: ExprEff r => Text -> Eff r VarRes
findVar = \case
  "super" ->
    findClassVar "self" >>= \case
      Nothing -> error "self not found when looking for super"
      Just fi -> pure $ SuperVar fi
  var ->
    findBlockVar var >>= \case
      Just l -> pure $ uncurry LocalVar l
      Nothing ->
        findClassVar var >>= \case
          Just f -> pure $ FieldVar f
          Nothing -> GlobalVar <$> findGlobalVar var

-- | Find variable in global context
findGlobalVar :: Member (State GlobalCtx) r => Text -> Eff r GlobalIx
findGlobalVar var = do
  ctx@MkGlobalCtx {globals} <- get
  let (newGlobs, idx) = LM.getOrSet var globals
  put (ctx {globals = newGlobs})
  pure idx

-- | Find variable in class context
findClassVar :: Member (Reader ClassCtx) r => Text -> Eff r (Maybe FieldIx)
findClassVar var = do
  MkClassCtx {fields} <- ask
  pure (Map.lookup var fields)

-- | Find variable in block context
findBlockVar :: Member (Reader BlockCtx) r => Text -> Eff r (Maybe (LocalIx, LocalIx))
findBlockVar var = findBlockPure <$> ask
  where
    findBlockPure MethodCtx {locals} = (0,) <$> LM.get var locals
    findBlockPure NestedBlockCtx {previous, locals} =
      ((0,) <$> LM.get var locals) -- current environment
        <|> (Bf.first (+ 1) <$> findBlockPure previous) -- higher environment
