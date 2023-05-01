{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module HaSOM.Compiler (compile, CompilationResult (..)) where

import Combinator ((.>))
import Control.Eff
import Control.Eff.ExcT
import Control.Eff.Exception
import Control.Eff.Reader.Strict
import Control.Eff.State.Strict
import Control.Monad (forM)
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as Map
import Data.List (intercalate, singleton)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.LookupMap as LM
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text.Utility ((<+), showT)
import HaSOM.AST as AST
import HaSOM.AST.Algebra
import HaSOM.Compiler.Context
import HaSOM.VM.Object hiding (getLiteral, locals)
import HaSOM.VM.Primitive (compilePrimitives)
import HaSOM.VM.Primitive.Type (PrimitiveContainer)
import HaSOM.VM.Universe

------------------------------------------------------------
-- Compilation orchestration

-- | Result of compilation
data CompilationResult = MkCompilationResult
  { globals :: VMGlobalsNat,
    coreClasses :: CoreClasses,
    literals :: VMLiterals,
    heap :: [(ObjIx, VMObjectNat)],
    nilObj :: VMObjectNat
  }

-- | Compile the ASTs
compile :: [AST.Class] -> [PrimitiveContainer] -> Either Text CompilationResult
compile asts prims = fmap finish <$> run $ runError @Text $ evalMockGC $ do
  nilIx <- getIdx
  runState (initGlobalCtx nilIx) $ do
    compilePrimitives prims
    heap <- compileClassesPure asts
    cc <- compileCoreClasses
    (nilObj, globsHeap) <- compileGlobalObjects cc
    pure (heap ++ globsHeap, cc, nilObj)
  where
    initGlobalCtx nilIx = MkGlobalCtx {globals = newGlobals, literals = newLiterals, primitives = Map.empty, nilIx}
    finish ((heap, coreClasses, nilObj), MkGlobalCtx {globals, literals}) =
      MkCompilationResult
        { globals,
          coreClasses,
          literals,
          heap,
          nilObj
        }

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
  nilClass <- findGlobalVar "Nil"
  trueClass <- findGlobalVar "True"
  falseClass <- findGlobalVar "False"
  blockClass <- findGlobalVar "Block"
  block1Class <- findGlobalVar "Block1"
  block2Class <- findGlobalVar "Block2"
  block3Class <- findGlobalVar "Block3"
  pure MkCoreClasses {..}

compileGlobalObjects :: (ClassEff r, Member ExcT r, Member (State MockGC) r) => CoreClasses -> Eff r (VMObjectNat, [(ObjIx, VMObjectNat)])
compileGlobalObjects MkCoreClasses {trueClass, falseClass, systemClass, nilClass} = do
  MkGlobalCtx {nilIx} <- get
  let mkInstance idx name =
        get @GlobalCtx >>= HaSOM.Compiler.Context.globals .> getGlobal idx .> \case
          Nothing -> throwT $ "Could not find " <+ name <+ " class"
          Just (ClassGlobal cl) -> pure InstanceObject {clazz = cl, fields = newFields cl nilIx}
          Just (ObjectGlobal _) -> throwT $ "Trying to find class " <+ name <+ ", but found object instead"

  nil <- mkInstance nilClass "Nil"
  trueIdx <- getIdx
  true <- mkInstance trueClass "True"
  falseIdx <- getIdx
  false <- mkInstance falseClass "False"
  systemIdx <- getIdx
  system <- mkInstance systemClass "System"

  pure (nil, [(nilIx, nil), (trueIdx, true), (falseIdx, false), (systemIdx, system)])

------------------------------------------------------------
-- Compile pure

type PureClasses = (Map.HashMap Text (Either AST.Class PartialClass))

compileClassesPure :: (ClassEff r, Member ExcT r, Member (State MockGC) r) => [AST.Class] -> Eff r [(ObjIx, VMObjectNat)]
compileClassesPure initialAsts  = do
  let asts = map changeSupernameClass initialAsts
  -- The map of uncompiled classes
  let astMap = Map.fromList $ map (\ast -> (AST.name ast, Left ast)) asts
  -- Compile classes without meta class
  partials <- evalState @PureClasses astMap (forM asts runAst)
  -- Get the metaclass
  (metaclass, _, _, _) <- throwOnNothing "Class 'Metaclass' not defined" (lookup "Metaclass" partials)

  -- finish the classes
  concat <$> forM partials (finishClass metaclass)
  where
    runAst ast = do
      MkPartialClass {partialClasses} <- compileSingleClassPure [] ast
      (AST.name ast,) <$> (partialClasses <$> getIdx <*> getIdx)

    finishClass metaclass (name, (instanceClass, instanceAsObj, metaClass, partMetaAsObj)) = do
      -- Add instance class to globals
      classIx <- findGlobalVar name
      insertClass classIx instanceClass

      -- Add metaclass to globals
      metaIx <- findGlobalVar (addMeta name)
      insertClass metaIx metaClass

      -- Return the objects and their index in heap
      let metaAsObj = partMetaAsObj metaclass
      pure [(asObject instanceClass, instanceAsObj), (asObject metaClass, metaAsObj)]

    changeSupernameClass ast = ast {AST.superclass = actualSuperclass}
      where
        actualSuperclass = case AST.superclass ast of
          Nothing -> Just "Object"
          Just "nil" -> Nothing
          s -> s

compileSingleClassPure ::
  (Member (State PureClasses) r, Member ExcT r, ClassEff r) =>
  [Text] ->
  AST.Class ->
  Eff r PartialClass
compileSingleClassPure previous ast
  | classname `elem` previous = throwT $ "Cyclical inheritance: " <+ showT previous  -- Prevent cyclical inheritance
  | otherwise = do
      superC <- sequence $ AST.superclass ast <&> findClass

      let (superclassF, metasuperclassF) =
            case superC of
              Nothing -> (Map.empty, Map.empty)
              Just MkPartialClass {classFields, metaclassFields} -> (classFields, metaclassFields)

      partial <- fold compileAlgebra ast superclassF metasuperclassF
      modify @PureClasses $ Map.insert classname (Right partial)
      pure partial
  where
    classname = AST.name ast
    findClass supername =
      get >>= Map.lookup supername .> \case
        Nothing -> throwT $ "Could not find superclass " <+ supername
        Just (Left superAst) -> compileSingleClassPure (classname : previous) superAst
        Just (Right compiled) -> pure compiled

------------------------------------------------------------
-- Algebra results

data PartialClass = MkPartialClass
  { classFields :: FieldsLookup,
    metaclassFields :: FieldsLookup,
    partialClasses :: ObjIx -> ObjIx -> (VMClassNat, VMObjectNat, VMClassNat, VMClassNat -> VMObjectNat)
  }

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
    (FieldsLookup -> FieldsLookup -> Eff r PartialClass)
    (Eff (Reader ClassCtx : r) MethodRes)
    (Eff (Reader BlockCtx : Reader ClassCtx : r) BlockRes)
    (Eff (Reader BlockCtx : Reader ClassCtx : r) ExprRes)
compileAlgebra = MkAlgebra {..}
  where
    clazz name supername instanceF instanceM classF classM superclassF metasuperclassF = do
      (classFields, partialInstanceClass, partialInstanceAsObj) <-
        compileClass
          name
          supername
          instanceF
          instanceM
          superclassF

      (metaclassFields, partialMetaclassClass, partialMetaclassAsObj) <-
        compileClass
          (addMeta name)
          (addMeta <$> supername)
          classF
          classM
          metasuperclassF

      let partialClasses idx1 idx2 =
            let inClassC = partialInstanceClass idx1
                metaClassC = partialMetaclassClass idx2
                inAsObjC = partialInstanceAsObj metaClassC
             in (inClassC, inAsObjC, metaClassC, partialMetaclassAsObj)

      pure MkPartialClass {..}

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
        SuperVar li li' -> super [PUSH_LOCAL li li']

    literal = (expr . singleton . PUSH_LITERAL <$>) . transformLiteral

------------------------------------------------------------
-- Class compile

compileClass ::
  ClassEff r =>
  Identifier ->
  Maybe Identifier ->
  [Variable] ->
  [Eff (Reader ClassCtx : r) MethodRes] ->
  FieldsLookup ->
  Eff r (FieldsLookup, ObjIx -> VMClassNat, VMClassNat -> VMObjectNat)
compileClass name supername thisFields methods superFields = do
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

  -- Get this class ix
  thisClassIx <- findGlobalVar name

  let clazz asObjectIx =
        MkVMClass
          { fieldsCount = length fields,
            superclass,
            asObject = asObjectIx,
            methods = cMethods
          }
  let object asObjectClass =
        ClassObject
          { clazz = asObjectClass,
            fields = newFields asObjectClass nilIx,
            classOf = thisClassIx
          }

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
      SuperVar li li' -> SET_LOCAL li li'
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

transformLiteral :: Member (State GlobalCtx) r => Literal -> Eff r LiteralIx
transformLiteral (LArray lits) =
  mapM transformLiteral lits
    >>= getLiteral . ArrayLiteral
transformLiteral (LSymbol sym) = getLiteral (SymbolLiteral sym)
transformLiteral (LString str) = getLiteral (StringLiteral str)
transformLiteral (LInteger int) = getLiteral (IntLiteral int)
transformLiteral (LDouble d) = getLiteral (DoubleLiteral d)

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
