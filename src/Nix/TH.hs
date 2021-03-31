{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-missing-fields #-}

module Nix.TH where

import           Data.Fix                       ( Fix(..) )
import           Data.Generics.Aliases          ( extQ )
import qualified Data.Set                      as Set
import           Language.Haskell.TH
import qualified Language.Haskell.TH.Syntax    as TH
import           Language.Haskell.TH.Quote
import           Nix.Atoms
import           Nix.Expr
import           Nix.Parser

quoteExprExp :: String -> ExpQ
quoteExprExp s = do
  expr <-
    either
      (fail . show)
      pure
      (parseNixText $ toText s)
  dataToExpQ
    (const Nothing `extQ` metaExp (freeVars expr) `extQ` (pure . liftText))
    expr
 where
  liftText :: Text -> Q Exp
  liftText txt = AppE (VarE 'id) <$> TH.lift txt

quoteExprPat :: String -> PatQ
quoteExprPat s = do
  expr <-
    either
      (fail . show)
      pure
      (parseNixText $ toText s)
  dataToPatQ
    (const Nothing `extQ` metaPat (freeVars expr))
    expr

freeVars :: NExpr -> Set VarName
freeVars e = case unFix e of
  (NConstant    _               ) -> mempty
  (NStr         string          ) -> foldMap freeVars string
  (NSym         var             ) -> Set.singleton var
  (NList        list            ) -> foldMap freeVars list
  (NSet   NNonRecursive bindings) -> foldMap bindFree bindings
  (NSet   NRecursive bindings   ) -> Set.difference (foldMap bindFree bindings) (foldMap bindDefs bindings)
  (NLiteralPath _               ) -> mempty
  (NEnvPath     _               ) -> mempty
  (NUnary       _    expr       ) -> freeVars expr
  (NBinary      _    left right ) -> Set.union (freeVars left) (freeVars right)
  (NSelect      expr path orExpr) ->
    Set.unions
      [ freeVars expr
      , pathFree path
      , maybe mempty freeVars orExpr
      ]
  (NHasAttr expr            path) -> Set.union (freeVars expr) (pathFree path)
  (NAbs     (Param varname) expr) -> Set.delete varname (freeVars expr)
  (NAbs (ParamSet set _ varname) expr) ->
    Set.union
      -- Include all free variables from the expression and the default arguments
      (freeVars expr)
      -- But remove the argument name if existing, and all arguments in the parameter set
      (Set.difference
        (Set.unions (mapMaybe (fmap freeVars . snd) set))
        (Set.difference
          (maybe mempty Set.singleton varname)
          (Set.fromList (fmap fst set))
        )
      )
  (NLet            bindings expr) ->
    Set.union
      (freeVars expr)
      (Set.difference
        (foldMap bindFree bindings)
        (foldMap bindDefs bindings)
      )
  (NIf          cond th   el    ) ->
    Set.unions $ freeVars <$> [cond, th, el]
  -- Evaluation is needed to find out whether x is a "real" free variable in `with y; x`, we just include it
  -- This also makes sense because its value can be overridden by `x: with y; x`
  (NWith        set  expr       ) -> Set.union (freeVars set      ) (freeVars expr)
  (NAssert      assertion expr  ) -> Set.union (freeVars assertion) (freeVars expr)
  (NSynHole _                   ) -> mempty

 where

  staticKey :: NKeyName r -> Maybe VarName
  staticKey (StaticKey  varname) = pure varname
  staticKey (DynamicKey _      ) = mempty

  bindDefs :: Binding r -> Set VarName
  bindDefs (Inherit   Nothing                  _    _) = mempty
  bindDefs (Inherit  (Just _                 ) keys _) = Set.fromList $ mapMaybe staticKey keys
  bindDefs (NamedVar (StaticKey  varname :| _) _    _) = Set.singleton varname
  bindDefs (NamedVar (DynamicKey _       :| _) _    _) = mempty

  bindFree :: Binding NExpr -> Set VarName
  bindFree (Inherit  Nothing     keys _) = Set.fromList $ mapMaybe staticKey keys
  bindFree (Inherit (Just scope) _    _) = freeVars scope
  bindFree (NamedVar path        expr _) = Set.union (pathFree path) (freeVars expr)

  pathFree :: NAttrPath NExpr -> Set VarName
  pathFree = foldMap (foldMap freeVars)


class ToExpr a where
  toExpr :: a -> NExprLoc

instance ToExpr NExprLoc where
  toExpr = id

instance ToExpr VarName where
  toExpr = Fix . NSym_ nullSpan

instance ToExpr Int where
  toExpr = Fix . NConstant_ nullSpan . NInt . fromIntegral

instance ToExpr Integer where
  toExpr = Fix . NConstant_ nullSpan . NInt

instance ToExpr Float where
  toExpr = Fix . NConstant_ nullSpan . NFloat

metaExp :: Set VarName -> NExprLoc -> Maybe ExpQ
metaExp fvs (Fix (NSym_ _ x)) | x `Set.member` fvs =
  pure [| toExpr $(varE (mkName $ toString x)) |]
metaExp _ _ = Nothing

metaPat :: Set VarName -> NExprLoc -> Maybe PatQ
metaPat fvs (Fix (NSym_ _ x)) | x `Set.member` fvs =
  pure (varP (mkName $ toString x))
metaPat _ _ = Nothing

-- Use of @QuasiQuoter@ requires @String@.
-- After @Text -> String@ migrations done, _maybe_ think to use @QuasiText@.
nix :: QuasiQuoter
nix = QuasiQuoter { quoteExp = quoteExprExp, quotePat = quoteExprPat }
