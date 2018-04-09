{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-missing-fields #-}

module Nix.TH where

import           Data.Fix
import           Data.Generics.Aliases
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Nix.Expr
import           Nix.Parser

quoteExprExp :: String -> ExpQ
quoteExprExp s = do
    expr <- case parseNixString s of
        Failure err -> fail $ show err
        Success e   -> return e
    dataToExpQ (const Nothing `extQ` metaExp (freeVars expr)) expr

freeVars :: NExpr -> Set VarName
freeVars = error "NYI: Implement an evaluator to find free variables"

metaExp :: Set VarName -> NExpr -> Maybe ExpQ
metaExp fvs (Fix (NSym x)) | x `Set.member` fvs =
  Just [| toExpr $(varE (mkName (Text.unpack x))) |]
metaExp _ _ = Nothing

nix :: QuasiQuoter
nix = QuasiQuoter
    { quoteExp = quoteExprExp
    }
