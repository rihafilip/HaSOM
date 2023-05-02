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

    -- * Mock GC
    MockGC,
    evalMockGC,
    getIdx,

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
    insertClass,
    insertGlobalObject,

    -- * Variable lookup helpers
    VarRes (..),
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
import Control.Eff.Utility (modifyYield)
import qualified Data.Bifunctor as Bf
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as Map
import Data.LookupMap (LookupMap)
import qualified Data.LookupMap as LM
import Data.Text (Text)
import HaSOM.VM.Object hiding (getLiteral)
import HaSOM.VM.Universe

------------------------------------------------------------
-- Compilation contexts

data GlobalCtx = MkGlobalCtx
  { globals :: VMGlobalsNat,
    literals :: VMLiterals,
    primitives :: Map.HashMap (Text, Text) NativeFun,
    nilIx :: ObjIx
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
-- Mock GC

newtype MockGC = MkMockGC Int

evalMockGC :: Eff (State MockGC : r) a -> Eff r a
evalMockGC = evalState (MkMockGC 1)

getIdx :: Member (State MockGC) r => Eff r ObjIx
getIdx = modifyYield $ \(MkMockGC i) -> (MkMockGC (i + 1), ix i)

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
  ctx@MkGlobalCtx {literals} <- get
  let (newLit, litIx) = internLiteral sym literals
  put (ctx {literals = newLit})
  pure litIx

------------------------------------------------------------
-- Class insertion

insertClass :: Member (State GlobalCtx) r => GlobalIx -> VMClassNat -> Eff r ()
insertClass idx clazz = do
  ctx@MkGlobalCtx {globals} <- get
  let newGlobs = setGlobal idx (ClassGlobal clazz) globals
  put (ctx {globals = newGlobs})

insertGlobalObject :: Member (State GlobalCtx) r => GlobalIx -> ObjIx -> Eff r ()
insertGlobalObject idx obj = do
  ctx@MkGlobalCtx {globals} <- get
  let newGlobs = setGlobal idx (ObjectGlobal obj) globals
  put (ctx {globals = newGlobs})

------------------------------------------------------------
-- Variable resolving

-- | Varibale lookup result
data VarRes
  = GlobalVar GlobalIx
  | FieldVar FieldIx
  | SuperVar LocalIx LocalIx
  | LocalVar LocalIx LocalIx -- EnvironmentIx, Local

-- | Find variable
findVar :: ExprEff r => Text -> Eff r VarRes
findVar = \case
  "super" ->
    findBlockVar "self" >>= \case
      Nothing -> error "self not found when looking for super"
      Just (li, li') -> pure $ SuperVar li li'
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
  let (newGlobs, idx) = internGlobal var globals
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
