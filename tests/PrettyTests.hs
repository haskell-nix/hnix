{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module PrettyTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import Nix.Expr
import Nix.Pretty

case_indented_antiquotation :: Assertion
case_indented_antiquotation = do
    assertPretty (mkIndentedStr "echo $foo") "''echo $foo''"
    assertPretty (mkIndentedStr "echo ${foo}") "''echo ''${foo}''"

case_string_antiquotation :: Assertion
case_string_antiquotation = do
    -- TODO: plain $ doesn't need to be escaped here either
    assertPretty (mkStr "echo $foo") "\"echo \\$foo\""
    assertPretty (mkStr "echo ${foo}") "\"echo \\${foo}\""

tests :: TestTree
tests = $testGroupGenerator

--------------------------------------------------------------------------------
assertPretty :: NExpr -> String -> Assertion
assertPretty e s = assertEqual ("When pretty-printing " ++ show e) s . show $ prettyNix e
