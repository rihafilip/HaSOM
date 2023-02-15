{-# LANGUAGE ScopedTypeVariables #-}

-- | Definition of fold algebra for AST
module HaSOM.AST.Algebra (Algebra (..), fold) where

import Data.List.NonEmpty (NonEmpty)
import HaSOM.AST

-- | Type of an algebra on AST
--
-- The type arguments are the types of output on individual nodes:
--
--  c -> class
--
--  m -> method
--
--  b -> block
--
--  e -> expression
data Algebra c m b e = MkAlgebra
  { -- | Fold on class (name, Maybe superclass, instance fields, methods, class fields, class methods)
    clazz :: Identifier -> Maybe Identifier -> [Variable] -> [m] -> [Variable] -> [m] -> c,
    -- | Fold on method (Maybe b is Just b on method block and Nothing on primitive method)
    method :: MethodType -> Maybe b -> m,
    -- | Fold on block (locals, the contained expressions)
    block :: [Variable] -> [e] -> b,
    -- | Fold on exit expression
    exit :: e -> e,
    -- | Fold on assign expression
    assign :: NonEmpty Variable -> e -> e,
    -- | Fold on unary call
    unCall :: e -> UnarySelector -> e,
    -- | Fold on binary call
    binCall :: e -> BinarySelector -> e -> e,
    -- | Fold on keyword call
    kwCall :: e -> NonEmpty (Keyword, e) -> e,
    -- | Fold on variable expression
    variableExpr :: Variable -> e,
    -- | Fold on nested block
    nestedBlock :: [Variable] -> b -> e,
    -- | Fold on literal
    literal :: Literal -> e
  }

-- | Fold function to apply algebra to a Class
fold :: forall c m b e. Algebra c m b e -> Class -> c
fold MkAlgebra {..} = fc
  where
    mapm :: Functor f => f Method -> f m
    mapm = fmap fm
    mape :: Functor f => f Expression -> f e
    mape = fmap fe

    fc MkClass {name, superclass, instanceFields, instanceMethods, classFields, classMethods} =
      clazz name superclass instanceFields (mapm instanceMethods) classFields (mapm classMethods)
    fm (MkMethod t b) =
      let b' = case b of
            MethodPrimitive -> Nothing
            MethodBlock bl -> Just (fb bl)
       in method t b'
    fb (MkBlock vs es) = block vs (mape es)

    fe (Exit expr) = exit (fe expr)
    fe (Assignation vs expr) = assign vs (fe expr)
    fe (UnaryCall expr un) = unCall (fe expr) un
    fe (BinaryCall expr1 bin expr2) = binCall (fe expr1) bin (fe expr2)
    fe (KeywordCall expr kws) =
      let kws' = fmap (\(MkKeywordMessage kw e) -> (kw, fe e)) kws
       in kwCall (fe expr) kws'
    fe (PrimaryVariable var) = variableExpr var
    fe (PrimaryBlock (MkNestedBlock params b)) = nestedBlock params (fb b)
    fe (PrimaryLiteral lit) = literal lit
