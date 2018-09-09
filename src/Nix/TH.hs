{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -Wno-missing-fields #-}

module Nix.TH where

import           Data.Fix
import           Data.Foldable
import           Data.Generics.Aliases
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Nix.Atoms
import           Nix.Expr
import           Nix.Parser

quoteExprExp :: String -> ExpQ
quoteExprExp s = do
    expr <- case parseNixTextLoc (Text.pack s) of
        Failure err -> fail $ show err
        Success e   -> return e
    dataToExpQ (const Nothing `extQ` metaExp (freeVars expr)) expr

quoteExprPat :: String -> PatQ
quoteExprPat s = do
    expr <- case parseNixTextLoc (Text.pack s) of
        Failure err -> fail $ show err
        Success e   -> return e
    dataToPatQ (const Nothing `extQ` metaPat (freeVars expr)) expr

freeVars :: NExprLoc -> Set VarName
freeVars = cata $ \case
    NSym_ _ var -> Set.singleton var
    Compose (Ann _ x) -> fold x

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
    Just [| toExpr $(varE (mkName (Text.unpack x))) |]
metaExp _ _ = Nothing

metaPat :: Set VarName -> NExprLoc -> Maybe PatQ
metaPat fvs (Fix (NSym_ _ x)) | x `Set.member` fvs =
    Just (varP (mkName (Text.unpack x)))
metaPat _ _ = Nothing

nix :: QuasiQuoter
nix = QuasiQuoter
    { quoteExp = quoteExprExp
    , quotePat = quoteExprPat
    }
