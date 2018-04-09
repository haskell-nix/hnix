{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module ParserTests (tests) where

import           Data.Fix
import qualified Data.HashMap.Strict.InsOrd as OM
import           Data.Text (pack)
import           Nix.Atoms
import           Nix.Expr
import           Nix.Parser
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.TH

case_constant_int :: Assertion
case_constant_int = assertParseString "234" $ mkInt 234

case_constant_bool :: Assertion
case_constant_bool = do
  assertParseString "true" $ mkBool True
  assertParseString "false" $ mkBool False

case_constant_bool_respects_attributes :: Assertion
case_constant_bool_respects_attributes = do
  assertParseString "true-foo"  $ mkSym "true-foo"
  assertParseString "false-bar" $ mkSym "false-bar"

case_constant_path :: Assertion
case_constant_path = do
  assertParseString "./." $ mkPath False "./."
  assertParseString "./+-_/cdef/09ad+-" $ mkPath False "./+-_/cdef/09ad+-"
  assertParseString "/abc" $ mkPath False "/abc"
  assertParseString "../abc" $ mkPath False "../abc"
  assertParseString "<abc>" $ mkPath True "abc"
  assertParseString "<../cdef>" $ mkPath True "../cdef"
  assertParseString "a//b" $ mkOper2 NUpdate (mkSym "a") (mkSym "b")
  assertParseString "rec+def/cdef" $ mkPath False "rec+def/cdef"
  assertParseString "a/b//c/def//<g> < def/d" $ mkOper2 NLt
    (mkOper2 NUpdate (mkPath False "a/b") $ mkOper2 NUpdate
      (mkPath False "c/def") (mkPath True "g"))
    (mkPath False "def/d")
  assertParseString "a'b/c" $ Fix $ NApp (mkSym "a'b") (mkPath False "/c")
  assertParseString "a/b" $ mkPath False "a/b"
  assertParseString "4/2" $ mkPath False "4/2"
  assertParseFail "."
  assertParseFail ".."
  assertParseFail "/"
  assertParseFail "a/"
  assertParseFail "a/def/"

case_constant_uri :: Assertion
case_constant_uri = do
  assertParseString "a:a" $ mkUri "a:a"
  assertParseString "http://foo.bar" $ mkUri "http://foo.bar"
  assertParseString "a+de+.adA+-:%%%ads%5asdk&/" $ mkUri "a+de+.adA+-:%%%ads%5asdk&/"
  assertParseString "rec+def:c" $ mkUri "rec+def:c"
  assertParseString "f.foo:bar" $ mkUri "f.foo:bar"
  assertParseFail "http://foo${\"bar\"}"
  assertParseFail ":bcdef"
  assertParseFail "a%20:asda"
  assertParseFail ".:adasd"
  assertParseFail "+:acdcd"

case_simple_set :: Assertion
case_simple_set = do
  assertParseString "{ a = 23; b = 4; }" $ Fix $ NSet
    [ NamedVar (mkSelector "a") $ mkInt 23
    , NamedVar (mkSelector "b") $ mkInt 4
    ]
  assertParseFail "{ a = 23 }"

case_set_inherit :: Assertion
case_set_inherit = do
  assertParseString "{ e = 3; inherit a b; }" $ Fix $ NSet
    [ NamedVar (mkSelector "e") $ mkInt 3
    , Inherit Nothing $ flip StaticKey Nothing <$> ["a", "b"]
    ]
  assertParseString "{ inherit; }" $ Fix $ NSet [ Inherit Nothing [] ]

case_set_scoped_inherit :: Assertion
case_set_scoped_inherit = assertParseString "{ inherit (a) b c; e = 4; inherit(a)b c; }" $ Fix $ NSet
  [ Inherit (Just (mkSym "a")) $ flip StaticKey Nothing <$> ["b", "c"]
  , NamedVar (mkSelector "e") $ mkInt 4
  , Inherit (Just (mkSym "a")) $ flip StaticKey Nothing <$> ["b", "c"]
  ]

case_set_rec :: Assertion
case_set_rec = assertParseString "rec { a = 3; b = a; }" $ Fix $ NRecSet
  [ NamedVar (mkSelector "a") $ mkInt 3
  , NamedVar (mkSelector "b") $ mkSym "a"
  ]

case_set_complex_keynames :: Assertion
case_set_complex_keynames = do
  assertParseString "{ \"\" = null; }" $ Fix $ NSet
    [ NamedVar [DynamicKey (Plain "")] mkNull ]
  assertParseString "{ a.b = 3; a.c = 4; }" $ Fix $ NSet
    [ NamedVar [StaticKey "a" Nothing, StaticKey "b" Nothing] $ mkInt 3
    , NamedVar [StaticKey "a" Nothing, StaticKey "c" Nothing] $ mkInt 4
    ]
  assertParseString "{ ${let a = \"b\"; in a} = 4; }" $ Fix $ NSet
    [ NamedVar [DynamicKey (Antiquoted letExpr)] $ mkInt 4 ]
  assertParseString "{ \"a${let a = \"b\"; in a}c\".e = 4; }" $ Fix $ NSet
    [ NamedVar [DynamicKey (Plain str), StaticKey "e" Nothing] $ mkInt 4 ]
 where
  letExpr = Fix $ NLet [ NamedVar (mkSelector "a") (mkStr "b") ] (mkSym "a")
  str = DoubleQuoted [Plain "a", Antiquoted letExpr, Plain "c"]

case_set_inherit_direct :: Assertion
case_set_inherit_direct = assertParseString "{ inherit ({a = 3;}); }" $ Fix $ NSet
  [ flip Inherit [] $ Just $ Fix $ NSet [NamedVar (mkSelector "a") $ mkInt 3]
  ]

case_inherit_selector :: Assertion
case_inherit_selector = do
  assertParseString "{ inherit \"a\"; }" $ Fix $ NSet
    [Inherit Nothing [DynamicKey (Plain "a")]]
  assertParseFail "{ inherit a.x; }"

case_int_list :: Assertion
case_int_list = assertParseString "[1 2 3]" $ Fix $ NList
  [ mkInt i | i <- [1,2,3] ]

case_int_null_list :: Assertion
case_int_null_list = assertParseString "[1 2 3 null 4]" $ Fix (NList (map (Fix . NConstant) [NInt 1, NInt 2, NInt 3, NNull, NInt 4]))

case_mixed_list :: Assertion
case_mixed_list = do
  assertParseString "[{a = 3;}.a (if true then null else false) null false 4 [] c.d or null]" $ Fix $ NList
    [ Fix (NSelect (Fix (NSet [NamedVar (mkSelector "a") (mkInt 3)])) (mkSelector "a") Nothing)
    , Fix (NIf (mkBool True) mkNull (mkBool False))
    , mkNull, mkBool False, mkInt 4, Fix (NList [])
    , Fix (NSelect (mkSym "c") (mkSelector "d") (Just mkNull))
    ]
  assertParseFail "[if true then null else null]"
  assertParseFail "[a ? b]"
  assertParseFail "[a : a]"
  assertParseFail "[${\"test\")]"

case_simple_lambda :: Assertion
case_simple_lambda = assertParseString "a: a" $ Fix $ NAbs (Param "a") (mkSym "a")

case_lambda_or_uri :: Assertion
case_lambda_or_uri = do
  assertParseString "a :b" $ Fix $ NAbs (Param "a") (mkSym "b")
  assertParseString "a c:def" $ Fix $ NApp (mkSym "a") (mkUri "c:def")
  assertParseString "c:def: c" $ Fix $ NApp (mkUri "c:def:") (mkSym "c")
  assertParseString "a:{}" $ Fix $ NAbs (Param "a") $ Fix $ NSet []
  assertParseString "a:[a]" $ Fix $ NAbs (Param "a") $ Fix $ NList [mkSym "a"]
  assertParseFail "def:"

case_lambda_pattern :: Assertion
case_lambda_pattern = do
  assertParseString "{b, c ? 1}: b" $
    Fix $ NAbs (fixed args Nothing) (mkSym "b")
  assertParseString "{ b ? x: x  }: b" $
    Fix $ NAbs (fixed args2 Nothing) (mkSym "b")
  assertParseString "a@{b,c ? 1}: b" $
    Fix $ NAbs (fixed args (Just "a")) (mkSym "b")
  assertParseString "{b,c?1}@a: c" $
    Fix $ NAbs (fixed args (Just "a")) (mkSym "c")
  assertParseString "{b,c?1,...}@a: c" $
    Fix $ NAbs (variadic vargs (Just "a")) (mkSym "c")
  assertParseString "{...}: 1" $
    Fix $ NAbs (variadic mempty Nothing) (mkInt 1)
  assertParseFail "a@b: a"
  assertParseFail "{a}@{b}: a"
 where
  fixed args = ParamSet args False
  variadic args = ParamSet args True
  args = OM.fromList [("b", Nothing), ("c", Just $ mkInt 1)]
  vargs = OM.fromList [("b", Nothing), ("c", Just $ mkInt 1)]
  args2 = OM.fromList [("b", Just lam)]
  lam = Fix $ NAbs (Param "x") (mkSym "x")

case_lambda_app_int :: Assertion
case_lambda_app_int = assertParseString "(a: a) 3" $ Fix (NApp lam int) where
  int = mkInt 3
  lam = Fix (NAbs (Param "a") asym)
  asym = mkSym "a"

case_simple_let :: Assertion
case_simple_let = do
  assertParseString "let a = 4; in a" $ Fix (NLet binds $ mkSym "a")
  assertParseFail "let a = 4 in a"
 where
  binds = [NamedVar (mkSelector "a") $ mkInt 4]

case_let_body :: Assertion
case_let_body = assertParseString "let { body = 1; }" letBody
  where
    letBody = Fix $ NSelect aset (mkSelector "body") Nothing
    aset = Fix $ NRecSet [NamedVar (mkSelector "body") (mkInt 1)]

case_nested_let :: Assertion
case_nested_let = do
  assertParseString "let a = 4; in let b = 5; in a" $ Fix $ NLet
    [ NamedVar (mkSelector "a") $ mkInt 4 ]
    (Fix $ NLet [NamedVar (mkSelector "b") $ mkInt 5] $ mkSym "a")
  assertParseFail "let a = 4; let b = 3; in b"

case_let_scoped_inherit :: Assertion
case_let_scoped_inherit = do
  assertParseString "let a = null; inherit (b) c; in c" $ Fix $ NLet
    [ NamedVar (mkSelector "a") mkNull
    , Inherit (Just $ mkSym "b") [StaticKey "c" Nothing] ]
    (mkSym "c")
  assertParseFail "let inherit (b) c in c"

case_if :: Assertion
case_if = do
  assertParseString "if true then true else false" $ Fix $ NIf (mkBool True) (mkBool True) (mkBool False)
  assertParseFail "if true then false"
  assertParseFail "else"
  assertParseFail "if true then false else"
  assertParseFail "if true then false else false else"
  assertParseFail "1 + 2 then"

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

case_identifier_keyword_prefix :: Assertion
case_identifier_keyword_prefix = do
  assertParseString "true-name" $ mkSym "true-name"
  assertParseString "trueName" $ mkSym "trueName"
  assertParseString "null-name" $ mkSym "null-name"
  assertParseString "nullName" $ mkSym "nullName"
  assertParseString "[ null-name ]" $ mkList [ mkSym "null-name" ]

makeStringParseTest :: String -> Assertion
makeStringParseTest str = assertParseString ("\"" ++ str ++ "\"") $ mkStr $ pack str

case_simple_string :: Assertion
case_simple_string = mapM_ makeStringParseTest ["abcdef", "a", "A", "   a a  ", ""]

case_string_dollar :: Assertion
case_string_dollar = mapM_ makeStringParseTest ["a$b", "a$$b", "$cdef", "gh$i"]

case_string_escape :: Assertion
case_string_escape = do
  assertParseString "\"\\$\\n\\t\\r\\\\\"" $ mkStr "$\n\t\r\\"
  assertParseString "\" \\\" \\' \"" $ mkStr " \" ' "

case_string_antiquote :: Assertion
case_string_antiquote = do
  assertParseString "\"abc${  if true then \"def\" else \"abc\"  } g\"" $
    Fix $ NStr $ DoubleQuoted
      [ Plain "abc"
      , Antiquoted $ Fix $ NIf (mkBool True) (mkStr "def") (mkStr "abc")
      , Plain " g"
      ]
  assertParseString "\"\\${a}\"" $ mkStr "${a}"
  assertParseFail "\"a"
  assertParseFail "${true}"
  assertParseFail "\"${true\""

case_select :: Assertion
case_select = do
  assertParseString "a .  e .di. f" $ Fix $ NSelect (mkSym "a")
    [ StaticKey "e" Nothing, StaticKey "di" Nothing, StaticKey "f" Nothing ]
    Nothing
  assertParseString "a.e . d    or null" $ Fix $ NSelect (mkSym "a")
    [ StaticKey "e" Nothing, StaticKey "d" Nothing ]
    (Just mkNull)
  assertParseString "{}.\"\"or null" $ Fix $ NSelect (Fix (NSet []))
    [ DynamicKey (Plain "") ] (Just mkNull)

case_select_path :: Assertion
case_select_path = do
  assertParseString "f ./." $ Fix $ NApp (mkSym "f") (mkPath False "./.")
  assertParseString "f.b ../a" $ Fix $ NApp select (mkPath False "../a")
  assertParseString "{}./def" $ Fix $ NApp (Fix (NSet [])) (mkPath False "./def")
  assertParseString "{}.\"\"./def" $ Fix $ NApp
    (Fix $ NSelect (Fix (NSet [])) [DynamicKey (Plain "")] Nothing)
    (mkPath False "./def")
 where select = Fix $ NSelect (mkSym "f") (mkSelector "b") Nothing

case_fun_app :: Assertion
case_fun_app = do
  assertParseString "f a b" $ Fix $ NApp (Fix $ NApp (mkSym "f") (mkSym "a")) (mkSym "b")
  assertParseString "f a.x or null" $ Fix $ NApp (mkSym "f") $ Fix $
    NSelect (mkSym "a") (mkSelector "x") (Just mkNull)
  assertParseFail "f if true then null else null"

case_indented_string :: Assertion
case_indented_string = do
  assertParseString "''a''" $ mkIndentedStr "a"
  assertParseString "''\n  foo\n  bar''" $ mkIndentedStr "foo\nbar"
  assertParseString "''        ''" $ mkIndentedStr ""
  assertParseString "'''''''" $ mkIndentedStr "''"
  assertParseString "''   ${null}\n   a${null}''" $ Fix $ NStr $ Indented
    [ Antiquoted mkNull
    , Plain "\na"
    , Antiquoted mkNull
    ]
  assertParseFail "'''''"
  assertParseFail "''   '"

case_indented_string_escape :: Assertion
case_indented_string_escape = assertParseString
  "'' ''\\n ''\\t ''\\\\ ''${ \\ \\n ' ''' ''" $
  mkIndentedStr  "\n \t \\ ${ \\ \\n ' '' "

case_operator_fun_app :: Assertion
case_operator_fun_app = do
  assertParseString "a ++ b" $ mkOper2 NConcat (mkSym "a") (mkSym "b")
  assertParseString "a ++ f b" $ mkOper2 NConcat (mkSym "a") $ Fix $ NApp
    (mkSym "f") (mkSym "b")

case_operators :: Assertion
case_operators = do
  assertParseString "1 + 2 - 3" $ mkOper2 NMinus
    (mkOper2 NPlus (mkInt 1) (mkInt 2)) (mkInt 3)
  assertParseFail "1 + if true then 1 else 2"
  assertParseString "1 + (if true then 2 else 3)" $ mkOper2 NPlus (mkInt 1) $ Fix $ NIf
   (mkBool True) (mkInt 2) (mkInt 3)
  assertParseString "{ a = 3; } // rec { b = 4; }" $ mkOper2 NUpdate
    (Fix $ NSet [NamedVar (mkSelector "a") (mkInt 3)])
    (Fix $ NRecSet [NamedVar (mkSelector "b") (mkInt 4)])
  assertParseString "--a" $ mkOper NNeg $ mkOper NNeg $ mkSym "a"
  assertParseString "a - b - c" $ mkOper2 NMinus
    (mkOper2 NMinus (mkSym "a") (mkSym "b")) $
    mkSym "c"
  assertParseString "foo<bar" $ mkOper2 NLt (mkSym "foo") (mkSym "bar")
  assertParseFail "+ 3"
  assertParseFail "foo +"

case_comments :: Assertion
case_comments = do
  Success expected <- parseNixFile "data/let.nix"
  assertParseFile "let-comments-multiline.nix" expected
  assertParseFile "let-comments.nix" expected

tests :: TestTree
tests = $testGroupGenerator

--------------------------------------------------------------------------------

assertParseString :: String -> NExpr -> Assertion
assertParseString str expected = case parseNixString str of
  Success actual ->
      assertEqual ("When parsing " ++ str)
          (stripPositionInfo expected) (stripPositionInfo actual)
  Failure err    ->
      assertFailure $ "Unexpected error parsing `" ++ str ++ "':\n" ++ show err

assertParseFile :: FilePath -> NExpr -> Assertion
assertParseFile file expected = do
  res <- parseNixFile $ "data/" ++ file
  case res of
    Success actual -> assertEqual ("Parsing data file " ++ file)
          (stripPositionInfo expected) (stripPositionInfo actual)
    Failure err    ->
        assertFailure $ "Unexpected error parsing data file `"
            ++ file ++ "':\n" ++ show err

assertParseFail :: String -> Assertion
assertParseFail str = case parseNixString str of
  Failure _ -> return ()
  Success r ->
      assertFailure $ "Unexpected success parsing `"
          ++ str ++ ":\nParsed value: " ++ show r
