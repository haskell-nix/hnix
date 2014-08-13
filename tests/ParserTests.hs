{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module ParserTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import Data.Text (pack)

import Nix.Types
import Nix.Parser

case_constant_int :: Assertion
case_constant_int = assertParseString "234" $ mkInt 234

case_constant_bool :: Assertion
case_constant_bool = do
  assertParseString "true" $ mkBool True
  assertParseString "false" $ mkBool False

case_simple_set :: Assertion
case_simple_set = assertParseString "{ a = 23; b = 4; }" $ Fix $ NSet NonRec
  [ NamedVar (mkSym "a") $ mkInt 23
  , NamedVar (mkSym "b") $ mkInt 4
  ]

case_int_list :: Assertion
case_int_list = assertParseString "[1 2 3]" $ Fix $ NList
  [ mkInt i | i <- [1,2,3] ]

case_int_null_list :: Assertion
case_int_null_list = assertParseString "[1 2 3 null 4]" $ Fix (NList (map (Fix . NConstant) [NInt 1, NInt 2, NInt 3, NNull, NInt 4]))

case_simple_lambda :: Assertion
case_simple_lambda = assertParseString "a: a" $ Fix (NAbs (Fix $ NArgs $ FormalName "a") (mkSym "a"))

case_lambda_app_int :: Assertion
case_lambda_app_int = assertParseString "(a: a) 3" $ Fix (NApp lam int) where
  int = mkInt 3
  lam = Fix (NAbs (Fix $ NArgs $ FormalName "a") asym)
  asym = mkSym "a"

case_simple_let :: Assertion
case_simple_let = assertParseString "let a = 4; in a" $ Fix (NLet binds asym) where
  binds = [NamedVar asym $ mkInt 4]
  asym = mkSym "a"

case_identifier_special_chars :: Assertion
case_identifier_special_chars = do
  assertParseString "_a" $ mkSym "_a"
  assertParseString "a_b" $ mkSym "a_b"
  assertParseString "a'b" $ mkSym "a'b"
  assertParseString "a''b" $ mkSym "a''b"
  assertParseString "a-b" $ mkSym "a-b"
  assertParseString "a--b" $ mkSym "a--b"
  assertParseString "a12a" $ mkSym "a12a"
  assertParseFail ".a"
  assertParseFail "'a"

makeStringParseTest :: String -> Assertion
makeStringParseTest str = assertParseString ("\"" ++ str ++ "\"") $ mkStr $ pack str

case_simple_string :: Assertion
case_simple_string = mapM_ makeStringParseTest ["abcdef", "a", "A"]

case_string_dollar :: Assertion
case_string_dollar = mapM_ makeStringParseTest ["a$b", "a$$b", "$cdef", "gh$i"]

case_string_escape :: Assertion
case_string_escape = assertParseString "\"\\$\\n\\t\\r\\\\\"" $ mkStr "$\n\t\r\\"

case_if :: Assertion
case_if = do
  assertParseString "if true then true else false" $ Fix $ NIf (mkBool True) (mkBool True) (mkBool False)
  assertParseFail "if true then false"
  assertParseFail "else"
  assertParseFail "if true then false else"
  assertParseFail "if true then false else false else"
  assertParseFail "1 + 2 then"

tests :: TestTree
tests = $testGroupGenerator

--------------------------------------------------------------------------------
assertParseString :: String -> NExpr -> Assertion
assertParseString str expected = case parseNixString str of
  Success actual -> assertEqual ("When parsing " ++ str) expected actual
  Failure err    -> assertFailure $ "Unexpected error parsing `" ++ str ++ "':\n" ++ show err

assertParseFail :: String -> Assertion
assertParseFail str = case parseNixString str of
  Failure _ -> return ()
  Success r -> assertFailure $ "Unexpected success parsing `" ++ str ++ ":\nParsed value:" ++ show r
