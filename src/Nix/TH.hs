{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

{-# options_ghc -Wno-missing-fields #-}

module Nix.TH where

import           Nix.Prelude
import           Data.Generics.Aliases          ( extQ )
import qualified Data.Set                      as Set
import           Language.Haskell.TH
import qualified Language.Haskell.TH.Syntax    as TH
import           Language.Haskell.TH.Quote
import           Nix.Atoms
import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated
import           Nix.Parser

quoteExprExp :: String -> ExpQ
quoteExprExp s =
  do
    expr <- parseExpr $ fromString s
    dataToExpQ
      (extQOnFreeVars metaExp expr `extQ` (pure . (TH.lift :: Text -> ExpQ)))
      expr

quoteExprPat :: String -> PatQ
quoteExprPat s =
  do
    expr <- parseExpr @Q $ fromString s
    dataToPatQ
      (extQOnFreeVars @_ @NExprLoc @PatQ metaPat expr)
      expr


-- | Helper function.
extQOnFreeVars
  :: ( Typeable b
    , Typeable loc
    )
  => ( Set VarName
    -> loc
    -> Maybe q
    )
  -> NExpr
  -> b
  -> Maybe q
extQOnFreeVars f = extQ (const Nothing) . f . getFreeVars

class ToExpr a where
  toExpr :: a -> NExprLoc

instance ToExpr NExprLoc where
  toExpr = id

instance ToExpr VarName where
  toExpr = NSymAnn nullSpan Unknown

instance ToExpr Int where
  toExpr = NConstantAnn nullSpan . NInt . fromIntegral

instance ToExpr Integer where
  toExpr = NConstantAnn nullSpan . NInt

instance ToExpr Float where
  toExpr = NConstantAnn nullSpan . NFloat

metaExp :: Set VarName -> NExprLoc -> Maybe ExpQ
metaExp fvs (NSymAnn _ _ x) | x `Set.member` fvs =
  pure [| toExpr $(varE (mkName $ toString x)) |]
metaExp _ _ = Nothing

metaPat :: Set VarName -> NExprLoc -> Maybe PatQ
metaPat fvs (NSymAnn _ _ x) | x `Set.member` fvs =
  pure $ varP $ mkName $ toString x
metaPat _ _ = Nothing

-- Use of @QuasiQuoter@ requires @String@.
-- After @Text -> String@ migrations done, _maybe_ think to use @QuasiText@.
nix :: QuasiQuoter
nix = QuasiQuoter { quoteExp = quoteExprExp, quotePat = quoteExprPat }
