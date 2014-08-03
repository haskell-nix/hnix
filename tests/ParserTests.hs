{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module ParserTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import Nix.Types
import Nix.Parser

case_constant_int :: Assertion
case_constant_int = assertParseString "234" $ Fix (NConstant (NInt 234))

case_simple_set :: Assertion
case_simple_set = assertParseString "{ a = 23; b = 4; }" $ Fix $ NSet NonRec
  [ (Fix (NConstant (NSym "a")), Fix (NConstant (NInt 23)))
  , (Fix (NConstant (NSym "b")), Fix (NConstant (NInt 4)))
  ]

case_int_list :: Assertion
case_int_list = assertParseString "[1 2 3]" $ Fix $ NList
  [ Fix (NConstant (NInt i)) | i <- [1,2,3] ]

case_int_null_list :: Assertion
case_int_null_list = assertParseString "[1 2 3 null 4]" $ Fix (NList (map (Fix . NConstant) [NInt 1, NInt 2, NInt 3, NNull, NInt 4]))

case_simple_lambda :: Assertion
case_simple_lambda = assertParseString "a: a" $ Fix (NAbs asym asym) where
  asym = Fix (NConstant (NSym "a"))

case_lambda_app_int :: Assertion
case_lambda_app_int = assertParseString "(a:a) 3" $ Fix (NApp lam int) where
  int = Fix (NConstant (NInt 3))
  lam = Fix (NAbs asym asym)
  asym = Fix (NConstant (NSym "a"))

case_simple_let :: Assertion
case_simple_let = assertParseString "let a = 4; in a" $ Fix (NLet binds asym) where
  binds = [(asym, Fix (NConstant (NInt 4)))]
  asym = Fix (NConstant (NSym "a"))

case_identifier_special_chars :: Assertion
case_identifier_special_chars = do
  assertParseString "_a" $ Fix (NConstant (NSym "_a"))
  assertParseString "a_b" $ Fix (NConstant (NSym "a_b"))
  assertParseString "a'b" $ Fix (NConstant (NSym "a'b"))
  assertParseString "a''b" $ Fix (NConstant (NSym "a''b"))
  assertParseString "a-b" $ Fix (NConstant (NSym "a-b"))
  assertParseString "a--b" $ Fix (NConstant (NSym "a--b"))
  assertParseString "a12a" $ Fix (NConstant (NSym "a12a"))
  assertParseFail ".a"
  assertParseFail "'a"

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
