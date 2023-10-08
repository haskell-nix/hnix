{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Nix.TH where

import           Data.Fix                       ( Fix(Fix) )
import           Data.Generics.Aliases          ( extQ )
import qualified Data.Set                      as Set
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Nix.Atoms
import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated
import           Nix.Parser
import           Nix.Prelude

removeMissingNames :: Set VarName -> Q (Set VarName)
removeMissingNames =
  fmap Set.fromAscList
    . filterM (fmap isJust . lookupValueName . toString)
    . Set.toAscList

quoteExprExp :: String -> ExpQ
quoteExprExp s = do
  expr <- parseExpr $ fromString s
  vars <- removeMissingNames $ getFreeVars expr
  dataToExpQ (extQOnFreeVars metaExp vars) expr

quoteExprPat :: String -> PatQ
quoteExprPat s = do
  expr <- parseExpr @Q $ fromString s
  vars <- removeMissingNames $ getFreeVars expr
  dataToPatQ (extQOnFreeVars @_ @NExprLoc @PatQ metaPat vars) expr

-- | Helper function.
extQOnFreeVars
  :: (Typeable b, Typeable loc)
  => (Set VarName -> loc -> Maybe q)
  -> Set VarName
  -> b
  -> Maybe q
extQOnFreeVars f = extQ (const Nothing) . f

class ToExpr a where
  toExpr :: a -> NExpr

instance ToExpr NExpr where
  toExpr = id

instance ToExpr VarName where
  toExpr = Fix . NSym

instance {-# OVERLAPPING #-} ToExpr String where
  toExpr = Fix . NStr . fromString

instance ToExpr Text where
  toExpr = toExpr . toString

instance ToExpr Int where
  toExpr = Fix . NConstant . NInt . fromIntegral

instance ToExpr Bool where
  toExpr = Fix . NConstant . NBool

instance ToExpr Integer where
  toExpr = Fix . NConstant . NInt

instance ToExpr Float where
  toExpr = Fix . NConstant . NFloat

instance (ToExpr a) => ToExpr [a] where
  toExpr = Fix . NList . fmap toExpr

instance (ToExpr a) => ToExpr (NonEmpty a) where
  toExpr = toExpr . toList

instance ToExpr () where
  toExpr () = Fix $ NConstant NNull

instance (ToExpr a) => ToExpr (Maybe a) where
  toExpr = maybe (toExpr ()) toExpr

instance (ToExpr a, ToExpr b) => ToExpr (Either a b) where
  toExpr = either toExpr toExpr

metaExp :: Set VarName -> NExpr -> Maybe ExpQ
metaExp fvs (Fix (NSym x)) | x `Set.member` fvs =
  pure [| toExpr $(varE (mkName $ toString x)) |]
metaExp _ _ = Nothing

metaPat :: Set VarName -> NExprLoc -> Maybe PatQ
metaPat fvs (NSymAnn _ x) | x `Set.member` fvs =
  pure $ varP $ mkName $ toString x
metaPat _ _ = Nothing

-- Use of @QuasiQuoter@ requires @String@.
-- After @Text -> String@ migrations done, _maybe_ think to use @QuasiText@.
nix :: QuasiQuoter
nix = QuasiQuoter { quoteExp = quoteExprExp, quotePat = quoteExprPat }
