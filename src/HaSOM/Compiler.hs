{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module HaSOM.Compiler (compile) where

import Control.Applicative ((<|>))
import Control.Eff
import Control.Eff.Reader.Strict
import Control.Eff.State.Strict
import qualified Data.Bifunctor as Bf
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as Map
import Data.List (intercalate, singleton)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Text.Utility ((<+))
import HaSOM.AST as AST
import HaSOM.AST.Algebra
import HaSOM.Compiler.LookupMap
import HaSOM.VM.Primitive
import HaSOM.VM.Universe

------------------------------------------------------------
-- Compilation contexts

data GlobalCtx = MkGlobalCtx
  { globals :: LookupMap Text GlobalIx,
    symbols :: LookupMap VMLiteral LiteralIx,
    primitives :: Map.HashMap (Text, Text) NativeFun
  }

type Fields = Map.HashMap Text FieldIx

data ClassCtx = MkClassCtx
  { className :: Text,
    fields :: Fields
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
-- Context helpers

methodCtx :: [Text] -> BlockCtx
methodCtx = MethodCtx . fromListLM . ("self" :)

nestedBlockCtx :: [Text] -> BlockCtx -> BlockCtx
nestedBlockCtx = flip NestedBlockCtx . fromListLM . ("self" :)

isBlock :: Member (Reader BlockCtx) r => Eff r Bool
isBlock =
  ask <&> \case
    MethodCtx {} -> False
    NestedBlockCtx {} -> True

------------------------------------------------------------
-- Compilation orchestration

-- TODO primitives
compile :: [AST.Class] -> GCNat -> (VMGlobalsNat, Literals, GCNat)
compile = undefined -- TODO
  where
    partialClasses :: ClassEff r => [AST.Class] -> [ClassRes r]
    partialClasses = map (fold compileAlgebra)

    completeMap = undefined -- TODO

    fromPartial :: ClassEff r => Maybe Text -> PartialClass r -> Eff r CompleteClass
    fromPartial supername clazz =
      let fields = case supername of
            Nothing -> Map.empty
            Just n ->
              fst $
                fromMaybe (error "class not found") $
                  Map.lookup n completeMap
       in undefined -- TODO

------------------------------------------------------------
-- Algebra effects and results

-- Class
type ClassEff r = Member (State GlobalCtx) r

data ClassRes r = MkClassRes
  { name :: Text,
    supername :: Maybe Text,
    partialInstanceClass :: PartialClass r,
    partialClassClass :: PartialClass r
  }

-- superfields ->
type PartialClass r = ClassEff r => Fields -> ObjIx -> Eff r CompleteClass

type CompleteClass = (Fields, VMClassNat, VMObjectNat)

-- Method
type MethodEff r = [Reader ClassCtx, State GlobalCtx] <:: r

type MethodRes = Maybe (LiteralIx, VMMethodNat) -- Ix of method, compiled method

-- Block
type BlockEff r = [Reader ClassCtx, Reader BlockCtx, State GlobalCtx] <:: r

type BlockRes = (Int, Code) -- Locals count, code

-- Expression
type ExprEff r = [Reader ClassCtx, Reader BlockCtx, State GlobalCtx] <:: r

type ExprRes = (Bool, [Bytecode]) -- Is a super primary, code

------------------------------------------------------------
-- ExprRes helpers

exprR :: Applicative f => [Bytecode] -> f ExprRes
exprR = pure . (False,)

expr :: [Bytecode] -> ExprRes
expr = (False,)

super :: [Bytecode] -> ExprRes
super = (True,)

compileExpr :: Functor f => f ExprRes -> f [Bytecode]
compileExpr = (snd <$>)

sequenceCode :: Monad m => [m ExprRes] -> m [[Bytecode]]
sequenceCode es = fmap snd <$> sequence es

------------------------------------------------------------

compileAlgebra ::
  ClassEff r =>
  Algebra
    (ClassRes r)
    (Eff (Reader ClassCtx : r) MethodRes)
    (Eff (Reader BlockCtx : Reader ClassCtx : r) BlockRes)
    (Eff (Reader BlockCtx : Reader ClassCtx : r) ExprRes)
compileAlgebra = MkAlgebra {..}
  where
    clazz name mSupername instanceF instanceM classF classM =
      MkClassRes {name, supername, partialInstanceClass, partialClassClass}
      where
        addMeta = (<+ " class")

        supername =
          case mSupername of
            Nothing -> Just "Object"
            Just "nil" -> Nothing
            s -> s

        -- Instance class
        partialInstanceClass =
          compileClass
            name
            supername
            instanceF
            instanceM

        -- Class class
        partialClassClass =
          compileClass
            (addMeta name)
            (addMeta <$> supername)
            classF
            classM

    ---------------------------------------------------
    -- Native method
    method typ Nothing = do
      MkClassCtx {className} <- ask
      MkGlobalCtx {primitives} <- get

      -- signature
      let mName = methodTypeToText typ

      -- try to get native function
      let f = NativeMethod <$> Map.lookup (className, mName) primitives

      -- Literal for this method
      litIx <- getLiteral (SymbolLiteral mName)
      pure $ (litIx,) <$> f

    -- Compiled method
    method typ (Just b) = do
      -- signature
      let mName = methodTypeToText typ
      let params = methodTypeParams typ

      let bCtx = methodCtx params

      -- Eval
      (localCount, body) <- runReader bCtx b

      -- Literal for this method
      litIx <- getLiteral (SymbolLiteral mName)
      pure $
        Just
          ( litIx,
            BytecodeMethod
              { body,
                parameterCount = length params,
                localCount
              }
          )
    ---------------------------------------------------
    block vars es =
      -- Add local variables
      local (\ctx -> ctx {locals = putAllLM (locals ctx) vars}) $ do
        -- compile block
        bc <-
          isBlock >>= \case
            False -> compileMethodBlock es
            True -> compileNestedBlock es
        pure (length vars, fromListArray bc)

    ---------------------------------------------------
    -- Simple expressions
    exit nested = do
      cNested <- compileExpr nested
      cRet <-
        isBlock <&> \case
          False -> RETURN
          True -> NONLOCAL_RETURN
      exprR $ cNested ++ [cRet]

    assign as e = do
      cExpr <- compileExpr e
      cAssigns <- concat <$> mapM compileSingleAssign as
      exprR (cExpr ++ cAssigns)

    ---------------------------------------------------
    -- Calls
    unCall prim selector = do
      (isSuper, cPrim) <- prim
      cCall <- compileCall selector isSuper
      exprR $ cPrim ++ [cCall]

    binCall prim selector arg = do
      (isSuper, cPrim) <- prim
      cArg <- compileExpr arg
      cCall <- compileCall selector isSuper
      exprR $ cArg ++ cPrim ++ [cCall]

    kwCall prim kws = do
      let args = NonEmpty.toList $ fmap snd kws
      (isSuper, cPrim) <- prim
      cArgs <- concat <$> sequenceCode args
      cCall <- compileCall (keywordsToText kws) isSuper
      exprR $ cArgs ++ cPrim ++ [cCall]

    ---------------------------------------------------
    -- Nested Block
    nestedBlock params body = do
      -- Create local context function
      let nestedCtx = nestedBlockCtx params

      -- Compile the body
      (blockLocalCount, code) <- local nestedCtx body

      -- Make a block literal
      let b =
            MkVMBlock
              { blockBody = code,
                blockLocalCount,
                blockParameterCount = length params
              }

      -- Return the literal idx
      idx <- getLiteral (BlockLiteral b)
      exprR [PUSH_LITERAL idx]

    ---------------------------------------------------
    -- Simple Literals
    variableExpr var = do
      findVar var <&> \case
        GlobalVar gi -> expr [PUSH_GLOBAL gi]
        FieldVar fi -> expr [PUSH_FIELD fi]
        LocalVar li li' -> expr [PUSH_LOCAL li li']
        SuperVar li -> super [PUSH_FIELD li]

    literal (LArray arr) = exprR [] -- TODO
    literal (LSymbol sym) = compileLit (SymbolLiteral sym)
    literal (LString str) = compileLit (StringLiteral str)
    literal (LInteger int) = compileLit (IntLiteral int)
    literal (LDouble d) = compileLit (DoubleLiteral d)

------------------------------------------------------------
-- Class compile

compileClass ::
  ClassEff r =>
  Identifier ->
  Maybe Identifier ->
  [Variable] ->
  [Eff (Reader ClassCtx : r) MethodRes] ->
  PartialClass r
compileClass name supername thisFields methods superFields asObject = do
  -- Create fields
  let newIx = (+ 1) $ Map.foldl max 0 superFields
  let fields =
        foldr
          (uncurry Map.insert)
          superFields
          (("super", 0) : zip thisFields [newIx ..])

  -- Create context
  let cCtx = MkClassCtx {className = name, fields}

  -- Compile methods
  cMethods <- newMethods . catMaybes <$> runReader cCtx (sequence methods)

  -- Get superclass ix
  superclass <- maybe (pure Nothing) (fmap Just . findGlobalVar) supername

  -- Create class
  let clazz =
        MkVMClass
          { fieldsCount = length fields,
            superclass,
            asObject,
            methods = cMethods
          }

  -- TODO
  let object = undefined

  -- result
  pure (fields, clazz, object)

------------------------------------------------------------
-- Block compile

compileMethodBlock :: [Eff r ExprRes] -> Eff r [Bytecode]
compileMethodBlock es =
  (++ [PUSH_LOCAL 0 0, RETURN]) -- return self at the end
    . concatMap (++ [POP]) -- each expression is popped at the end
    <$> sequenceCode es -- compile expressions

compileNestedBlock :: [Eff r ExprRes] -> Eff r [Bytecode]
compileNestedBlock es =
  (++ [RETURN]) -- return the last expression
    . intercalate [POP] -- pop all expressions except the last one
    <$> sequenceCode es -- compile expressions

------------------------------------------------------------
-- Expression compile

compileSingleAssign :: ExprEff r => Variable -> Eff r [Bytecode]
compileSingleAssign var = do
  v <-
    findVar var <&> \case
      GlobalVar gi -> SET_GLOBAL gi
      FieldVar fi -> SET_FIELD fi
      SuperVar li -> SET_FIELD li
      LocalVar li li' -> SET_LOCAL li li'
  pure [DUP, v]

compileCall :: Member (State GlobalCtx) r => Text -> Bool -> Eff r Bytecode
compileCall selector isSuper =
  callBc <$> getLiteral (SymbolLiteral selector)
  where
    callBc
      | isSuper = SUPER_CALL
      | otherwise = CALL

------------------------------------------------------------
-- Literal compile

compileLit :: Member (State GlobalCtx) r => VMLiteral -> Eff r ExprRes
compileLit = (expr . singleton . PUSH_LITERAL <$>) . getLiteral

------------------------------------------------------------
-- Method signature

methodTypeToText :: MethodType -> Text
methodTypeToText (UnaryMethod t) = t
methodTypeToText (BinaryMethod t _) = t
methodTypeToText (KeywordMethod kws) = keywordsToText kws

methodTypeParams :: MethodType -> [Text]
methodTypeParams (UnaryMethod _) = []
methodTypeParams (BinaryMethod _ param) = [param]
methodTypeParams (KeywordMethod kws) = NonEmpty.toList $ fmap snd kws

keywordsToText :: NonEmpty (Keyword, a) -> Text
keywordsToText = foldl (<+) "" . fmap fst

------------------------------------------------------------
-- Literal resolving

getLiteral :: Member (State GlobalCtx) r => VMLiteral -> Eff r LiteralIx
getLiteral sym = do
  ctx@MkGlobalCtx {symbols} <- get
  let (newSym, symIx) = getOrSetLM symbols sym
  put (ctx {symbols = newSym})
  pure symIx

------------------------------------------------------------
-- Variable resolving

data VarRes
  = GlobalVar GlobalIx
  | FieldVar FieldIx
  | SuperVar FieldIx
  | LocalVar LocalIx LocalIx -- EnvironmentIx, Local

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

findGlobalVar :: Member (State GlobalCtx) r => Text -> Eff r GlobalIx
findGlobalVar var = do
  ctx@MkGlobalCtx {globals} <- get
  let (newGlobs, idx) = getOrSetLM globals var
  put (ctx {globals = newGlobs})
  pure idx

findClassVar :: Member (Reader ClassCtx) r => Text -> Eff r (Maybe FieldIx)
findClassVar var = do
  MkClassCtx {fields} <- ask
  pure (Map.lookup var fields)

findBlockVar :: Member (Reader BlockCtx) r => Text -> Eff r (Maybe (LocalIx, LocalIx))
findBlockVar var = findBlockPure <$> ask
  where
    findBlockPure MethodCtx {locals} = (0,) <$> getLM locals var
    findBlockPure NestedBlockCtx {previous, locals} =
      ((0,) <$> getLM locals var) -- current environment
        <|> (Bf.first (+ 1) <$> findBlockPure previous) -- higher environment
