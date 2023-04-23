{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module HaSOM.Compiler (compile, CompilationResult(..)) where

import Control.Eff
import Control.Eff.ExcT
import Control.Eff.Exception
import Control.Eff.Reader.Strict
import Control.Eff.State.Strict
import Control.Eff.Utility
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as Map
import Data.List (intercalate, singleton)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import Data.Text.Utility ((<+))
import HaSOM.AST as AST
import HaSOM.AST.Algebra
import HaSOM.Compiler.Context
import qualified HaSOM.Compiler.LookupMap as LM
import qualified HaSOM.VM.GC as GC
import HaSOM.VM.Object hiding (getLiteral, locals)
import HaSOM.VM.Primitive (compilePrimitives)
import HaSOM.VM.Primitive.Type (PrimitiveContainer)
import HaSOM.VM.Universe
import qualified Data.Bifunctor as Bf
import Data.Tuple (swap)

------------------------------------------------------------
-- Compilation orchestration

-- | Result of compilation
data CompilationResult = MkCompilationResult
  { globals :: VMGlobalsNat,
    coreClasses :: CoreClasses,
    literals :: VMLiterals,
    gCollector :: GCNat,
    globalsInterner :: Map.HashMap Text GlobalIx,
    symbolsInterner :: Map.HashMap Text LiteralIx
  }

-- | Compile the ASTs
compile :: [AST.Class] -> GCNat -> [PrimitiveContainer] -> Either Text CompilationResult
compile asts gc prims =
  fmap finish $
    run $
      runError @Text $
        runState initGlobalCtx $
          runState gc $ do
            compilePrimitives prims
            classes <- compileClasses asts
            coreClasses <- compileCoreClasses
            pure (classes, coreClasses)
  where
    initGlobalCtx = MkGlobalCtx {globals = LM.new, symbols = LM.new, primitives = Map.empty, nilIx = GC.nil gc}

    finish (((classes, coreClasses), gCollector), MkGlobalCtx {globals, symbols}) =
      MkCompilationResult
        { globals = newGlobals $ map (Bf.second ClassGlobal) classes,
          coreClasses,
          literals = newLiterals $ map swap $ LM.toList symbols,
          gCollector,
          globalsInterner = LM.toHashMap globals,
          symbolsInterner = Map.fromList $ mapMaybe extractSymbol $ LM.toList symbols
        }
    extractSymbol (SymbolLiteral s, v) = Just (s, v)
    extractSymbol _ = Nothing

compileClasses :: forall r. (ClassEff r, Member ExcT r, GCEff r) => [AST.Class] -> Eff r [(GlobalIx, VMClassNat)]
compileClasses asts = map (\(_, (_, idx, clazz)) -> (idx, clazz)) . Map.toList <$> completeMap
  where
    partialClasses :: [ClassRes r]
    partialClasses = map (fold compileAlgebra) asts

    completeMap :: Eff r (Map.HashMap Text (FieldsLookup, GlobalIx, VMClassNat))
    completeMap = Map.fromList . concat <$> mapM fromPartial partialClasses

    fromPartial :: ClassRes r -> Eff r [(Text, (FieldsLookup, GlobalIx, VMClassNat))]
    fromPartial MkClassRes {name, supername = mSupername, partialInstanceClass, partialClassClass} = do
      cmap <- completeMap

      classObjIx <- modifyYield @GCNat GC.new
      metaclassObjIx <- modifyYield @GCNat GC.new

      -- TODO throws
      lookupClass <- case mSupername of
        Nothing -> pure Map.empty
        Just supername -> throwOnNothing "" $ (\(x, _, _) -> x) <$> Map.lookup supername cmap
      lookupMetaclass <- case mSupername of
        Nothing -> pure Map.empty
        Just supername -> throwOnNothing "" $ (\(x, _, _) -> x) <$> Map.lookup (addMeta supername) cmap

      -- TODO
      metaclassClass <- throwOnNothing "" $ (\(_, _, x) -> x) <$> Map.lookup "Metaclass" cmap

      (metaLookup, metaGlobalIx, metaCompiled, metaAsObject) <-
        partialClassClass lookupMetaclass metaclassObjIx metaclassClass

      (classLookup, classGlobalIx, classCompiled, classAsObject) <-
        partialInstanceClass lookupClass classObjIx metaCompiled

      modify @GCNat $ GC.setAt classObjIx classAsObject
      modify @GCNat $ GC.setAt metaclassObjIx metaAsObject

      pure
        [ (name, (classLookup, metaGlobalIx, classCompiled)),
          (addMeta name, (metaLookup, classGlobalIx, metaCompiled))
        ]

compileCoreClasses :: ClassEff r => Eff r CoreClasses
compileCoreClasses = do
  classClass <- findGlobalVar "Class"
  metaclassClass <- findGlobalVar "Metaclass"
  objectClass <- findGlobalVar "Object"
  systemClass <- findGlobalVar "System"
  methodClass <- findGlobalVar "Method"
  primitiveClass <- findGlobalVar "Primitive"
  booleanClass <- findGlobalVar "Boolean"
  integerClass <- findGlobalVar "Integer"
  doubleClass <- findGlobalVar "Double"
  stringClass <- findGlobalVar "String"
  symbolClass <- findGlobalVar "Symbol"
  arrayClass <- findGlobalVar "Array"
  trueClass <- findGlobalVar "True"
  falseClass <- findGlobalVar "False"
  blockClass <- findGlobalVar "Block"
  block1Class <- findGlobalVar "Block1"
  block2Class <- findGlobalVar "Block2"
  block3Class <- findGlobalVar "Block3"
  pure MkCoreClasses {..}

------------------------------------------------------------
-- Algebra results

-- Class
data ClassRes r = MkClassRes
  { name :: Text,
    supername :: Maybe Text,
    partialInstanceClass :: PartialClass r,
    partialClassClass :: PartialClass r
  }

-- superfields -> new object index -> class of class-as-object -> compiled class
type PartialClass r =
  ClassEff r =>
  FieldsLookup ->
  ObjIx ->
  VMClassNat ->
  Eff r (FieldsLookup, GlobalIx, VMClassNat, VMObjectNat)

-- Method
type MethodRes = Maybe (LiteralIx, VMMethodNat) -- Ix of method, compiled method

-- Block
type BlockRes = (Int, Code) -- Locals count, code

-- Expression
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
      let (mName, params) = methodSignature typ

      -- try to get native function
      let f =
            Map.lookup (className, mName) primitives
              <&> \nativeBody ->
                NativeMethod
                  { nativeBody,
                    parameterCount = length params
                  }

      -- Literal for this method
      litIx <- getLiteral (SymbolLiteral mName)
      pure $ (litIx,) <$> f

    -- Compiled method
    method typ (Just b) = do
      -- signature
      let (mName, params) = methodSignature typ

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
      local (\ctx -> ctx {locals = LM.putAll vars (locals ctx)}) $ do
        -- compile block
        bc <-
          isBlock >>= \case
            False -> compileMethodBlock es
            True -> compileNestedBlock es
        pure (length vars, code bc)

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
      (blockLocalCount, blockBody) <- local nestedCtx body

      -- Make a block literal
      let b =
            MkVMBlock
              { blockBody,
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
compileClass name supername thisFields methods superFields asObjectIx asObjectClass = do
  MkGlobalCtx {nilIx} <- get

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

  -- Get superclass ix (if there is one)
  superclass <- traverse findGlobalVar supername

  -- Create class
  let clazz =
        MkVMClass
          { fieldsCount = length fields,
            superclass,
            asObject = asObjectIx,
            methods = cMethods
          }

  -- This class as object
  thisClassIx <- findGlobalVar name
  let object =
        ClassObject
          { clazz = asObjectClass,
            fields = newFields asObjectClass nilIx,
            classOf = thisClassIx
          }

  -- result
  pure (fields, thisClassIx, clazz, object)

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

methodSignature :: MethodType -> (Text, [Text])
methodSignature (UnaryMethod t) = (t, [])
methodSignature (BinaryMethod t param) = (t, [param])
methodSignature (KeywordMethod kws) =
  (keywordsToText kws, NonEmpty.toList $ fmap snd kws)

keywordsToText :: NonEmpty (Keyword, a) -> Text
keywordsToText = foldl (<+) "" . fmap fst

addMeta :: Text -> Text
addMeta = (<+ " class")
