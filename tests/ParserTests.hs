{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-orphans #-}

module ParserTests (tests) where

import Data.Fix
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup
import Data.String.Interpolate.IsString
import Data.Text (Text, unpack, pack)
import Nix.Atoms
import Nix.Expr
import Nix.Parser
import Nix.Pretty
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))

case_constant_int = assertParseText "234" $ mkInt 234

case_constant_bool = do
  assertParseText "true" $ mkBool True
  assertParseText "false" $ mkBool False

case_constant_bool_respects_attributes = do
  assertParseText "true-foo"  $ mkSym "true-foo"
  assertParseText "false-bar" $ mkSym "false-bar"

case_constant_path = do
  assertParseText "./." $ mkPath False "./."
  assertParseText "./+-_/cdef/09ad+-" $ mkPath False "./+-_/cdef/09ad+-"
  assertParseText "/abc" $ mkPath False "/abc"
  assertParseText "../abc" $ mkPath False "../abc"
  assertParseText "<abc>" $ mkPath True "abc"
  assertParseText "<../cdef>" $ mkPath True "../cdef"
  assertParseText "a//b" $ mkOper2 NUpdate (mkSym "a") (mkSym "b")
  assertParseText "rec+def/cdef" $ mkPath False "rec+def/cdef"
  assertParseText "a/b//c/def//<g> < def/d" $ mkOper2 NLt
    (mkOper2 NUpdate (mkPath False "a/b") $ mkOper2 NUpdate
      (mkPath False "c/def") (mkPath True "g"))
    (mkPath False "def/d")
  assertParseText "a'b/c" $ Fix $ NBinary NApp (mkSym "a'b") (mkPath False "/c")
  assertParseText "a/b" $ mkPath False "a/b"
  assertParseText "4/2" $ mkPath False "4/2"
  assertParseFail "."
  assertParseFail ".."
  assertParseFail "/"
  assertParseFail "a/"
  assertParseFail "a/def/"
  assertParseFail "~"
  assertParseFail "~/"
  assertParseText "~/a" $ mkPath False "~/a"
  assertParseText "~/a/b" $ mkPath False "~/a/b"

case_constant_uri = do
  assertParseText "a:a" $ mkStr "a:a"
  assertParseText "http://foo.bar" $ mkStr "http://foo.bar"
  assertParseText "a+de+.adA+-:%%%ads%5asdk&/" $ mkStr "a+de+.adA+-:%%%ads%5asdk&/"
  assertParseText "rec+def:c" $ mkStr "rec+def:c"
  assertParseText "f.foo:bar" $ mkStr "f.foo:bar"
  assertParseFail "http://foo${\"bar\"}"
  assertParseFail ":bcdef"
  assertParseFail "a%20:asda"
  assertParseFail ".:adasd"
  assertParseFail "+:acdcd"

case_simple_set = do
  assertParseText "{ a = 23; b = 4; }" $ Fix $ NSet
    [ NamedVar (mkSelector "a") (mkInt 23) nullPos
    , NamedVar (mkSelector "b") (mkInt 4) nullPos
    ]
  assertParseFail "{ a = 23 }"

case_set_inherit = do
  assertParseText "{ e = 3; inherit a b; }" $ Fix $ NSet
    [ NamedVar (mkSelector "e") (mkInt 3) nullPos
    , Inherit Nothing (StaticKey <$> ["a", "b"]) nullPos
    ]
  assertParseText "{ inherit; }" $ Fix $ NSet [ Inherit Nothing [] nullPos ]

case_set_scoped_inherit = assertParseText "{ inherit (a) b c; e = 4; inherit(a)b c; }" $ Fix $ NSet
  [ Inherit (Just (mkSym "a")) (StaticKey <$> ["b", "c"]) nullPos
  , NamedVar (mkSelector "e") (mkInt 4) nullPos
  , Inherit (Just (mkSym "a")) (StaticKey <$> ["b", "c"]) nullPos
  ]

case_set_rec = assertParseText "rec { a = 3; b = a; }" $ Fix $ NRecSet
  [ NamedVar (mkSelector "a") (mkInt 3) nullPos
  , NamedVar (mkSelector "b") (mkSym "a") nullPos
  ]

case_set_complex_keynames = do
  assertParseText "{ \"\" = null; }" $ Fix $ NSet
    [ NamedVar (DynamicKey (Plain (DoubleQuoted [])) :| []) mkNull nullPos ]
  assertParseText "{ a.b = 3; a.c = 4; }" $ Fix $ NSet
    [ NamedVar (StaticKey "a" :| [StaticKey "b"]) (mkInt 3) nullPos
    , NamedVar (StaticKey "a" :| [StaticKey "c"]) (mkInt 4) nullPos
    ]
  assertParseText "{ ${let a = \"b\"; in a} = 4; }" $ Fix $ NSet
    [ NamedVar (DynamicKey (Antiquoted letExpr) :| []) (mkInt 4) nullPos ]
  assertParseText "{ \"a${let a = \"b\"; in a}c\".e = 4; }" $ Fix $ NSet
    [ NamedVar (DynamicKey (Plain str) :| [StaticKey "e"]) (mkInt 4) nullPos ]
 where
  letExpr = Fix $ NLet [NamedVar (mkSelector "a") (mkStr "b") nullPos] (mkSym "a")
  str = DoubleQuoted [Plain "a", Antiquoted letExpr, Plain "c"]

case_set_inherit_direct = assertParseText "{ inherit ({a = 3;}); }" $ Fix $ NSet
  [ Inherit (Just $ Fix $ NSet [NamedVar (mkSelector "a") (mkInt 3) nullPos]) [] nullPos
  ]

case_inherit_selector = do
  assertParseText "{ inherit \"a\"; }" $ Fix $ NSet
    [Inherit Nothing [DynamicKey (Plain (DoubleQuoted [Plain "a"]))] nullPos]
  assertParseFail "{ inherit a.x; }"

case_int_list = assertParseText "[1 2 3]" $ Fix $ NList
  [ mkInt i | i <- [1,2,3] ]

case_int_null_list = assertParseText "[1 2 3 null 4]" $ Fix (NList (map (Fix . NConstant) [NInt 1, NInt 2, NInt 3, NNull, NInt 4]))

case_mixed_list = do
  assertParseText "[{a = 3;}.a (if true then null else false) null false 4 [] c.d or null]" $ Fix $ NList
    [ Fix (NSelect (Fix (NSet [NamedVar (mkSelector "a") (mkInt 3) nullPos]))
                   (mkSelector "a") Nothing)
    , Fix (NIf (mkBool True) mkNull (mkBool False))
    , mkNull, mkBool False, mkInt 4, Fix (NList [])
    , Fix (NSelect (mkSym "c") (mkSelector "d") (Just mkNull))
    ]
  assertParseFail "[if true then null else null]"
  assertParseFail "[a ? b]"
  assertParseFail "[a : a]"
  assertParseFail "[${\"test\")]"

case_simple_lambda = assertParseText "a: a" $ Fix $ NAbs (Param "a") (mkSym "a")

case_lambda_or_uri = do
  assertParseText "a :b" $ Fix $ NAbs (Param "a") (mkSym "b")
  assertParseText "a c:def" $ Fix $ NBinary NApp (mkSym "a") (mkStr "c:def")
  assertParseText "c:def: c" $ Fix $ NBinary NApp (mkStr "c:def:") (mkSym "c")
  assertParseText "a:{}" $ Fix $ NAbs (Param "a") $ Fix $ NSet []
  assertParseText "a:[a]" $ Fix $ NAbs (Param "a") $ Fix $ NList [mkSym "a"]
  assertParseFail "def:"

case_lambda_pattern = do
  assertParseText "{b, c ? 1}: b" $
    Fix $ NAbs (fixed args Nothing) (mkSym "b")
  assertParseText "{ b ? x: x  }: b" $
    Fix $ NAbs (fixed args2 Nothing) (mkSym "b")
  assertParseText "a@{b,c ? 1}: b" $
    Fix $ NAbs (fixed args (Just "a")) (mkSym "b")
  assertParseText "{b,c?1}@a: c" $
    Fix $ NAbs (fixed args (Just "a")) (mkSym "c")
  assertParseText "{b,c?1,...}@a: c" $
    Fix $ NAbs (variadic vargs (Just "a")) (mkSym "c")
  assertParseText "{...}: 1" $
    Fix $ NAbs (variadic mempty Nothing) (mkInt 1)
  assertParseFail "a@b: a"
  assertParseFail "{a}@{b}: a"
 where
  fixed args = ParamSet args False
  variadic args = ParamSet args True
  args = [("b", Nothing), ("c", Just $ mkInt 1)]
  vargs = [("b", Nothing), ("c", Just $ mkInt 1)]
  args2 = [("b", Just lam)]
  lam = Fix $ NAbs (Param "x") (mkSym "x")

case_lambda_app_int = assertParseText "(a: a) 3" $ Fix (NBinary NApp lam int) where
  int = mkInt 3
  lam = Fix (NAbs (Param "a") asym)
  asym = mkSym "a"

case_simple_let = do
  assertParseText "let a = 4; in a" $ Fix (NLet binds $ mkSym "a")
  assertParseFail "let a = 4 in a"
 where
  binds = [NamedVar (mkSelector "a") (mkInt 4) nullPos]

case_let_body = assertParseText "let { body = 1; }" letBody
  where
    letBody = Fix $ NSelect aset (mkSelector "body") Nothing
    aset = Fix $ NRecSet [NamedVar (mkSelector "body") (mkInt 1) nullPos]

case_nested_let = do
  assertParseText "let a = 4; in let b = 5; in a" $ Fix $ NLet
    [NamedVar (mkSelector "a") (mkInt 4) nullPos]
    (Fix $ NLet [NamedVar (mkSelector "b") (mkInt 5) nullPos] $ mkSym "a")
  assertParseFail "let a = 4; let b = 3; in b"

case_let_scoped_inherit = do
  assertParseText "let a = null; inherit (b) c; in c" $ Fix $ NLet
    [ NamedVar (mkSelector "a") mkNull nullPos
    , Inherit (Just $ mkSym "b") [StaticKey "c"] nullPos ]
    (mkSym "c")
  assertParseFail "let inherit (b) c in c"

case_if = do
  assertParseText "if true then true else false" $
      Fix $ NIf (mkBool True) (mkBool True) (mkBool False)
  assertParseFail "if true then false"
  assertParseFail "else"
  assertParseFail "if true then false else"
  assertParseFail "if true then false else false else"
  assertParseFail "1 + 2 then"

case_identifier_special_chars = do
  assertParseText "_a" $ mkSym "_a"
  assertParseText "a_b" $ mkSym "a_b"
  assertParseText "a'b" $ mkSym "a'b"
  assertParseText "a''b" $ mkSym "a''b"
  assertParseText "a-b" $ mkSym "a-b"
  assertParseText "a--b" $ mkSym "a--b"
  assertParseText "a12a" $ mkSym "a12a"
  assertParseFail ".a"
  assertParseFail "'a"

case_identifier_keyword_prefix = do
  assertParseText "true-name" $ mkSym "true-name"
  assertParseText "trueName" $ mkSym "trueName"
  assertParseText "null-name" $ mkSym "null-name"
  assertParseText "nullName" $ mkSym "nullName"
  assertParseText "[ null-name ]" $ mkList [ mkSym "null-name" ]

makeTextParseTest str = assertParseText ("\"" <> str <> "\"") $ mkStr str

case_simple_string = mapM_ makeTextParseTest ["abcdef", "a", "A", "   a a  ", ""]

case_string_dollar = mapM_ makeTextParseTest ["a$b", "a$$b", "$cdef", "gh$i"]

case_string_escape = do
  assertParseText "\"\\$\\n\\t\\r\\\\\"" $ mkStr "$\n\t\r\\"
  assertParseText "\" \\\" \\' \"" $ mkStr " \" ' "

case_string_antiquote = do
  assertParseText "\"abc${  if true then \"def\" else \"abc\"  } g\"" $
    Fix $ NStr $ DoubleQuoted
      [ Plain "abc"
      , Antiquoted $ Fix $ NIf (mkBool True) (mkStr "def") (mkStr "abc")
      , Plain " g"
      ]
  assertParseText "\"\\${a}\"" $ mkStr "${a}"
  assertParseFail "\"a"
  assertParseFail "${true}"
  assertParseFail "\"${true\""

case_select = do
  assertParseText "a .  e .di. f" $ Fix $ NSelect (mkSym "a")
    (StaticKey "e" :| [StaticKey "di", StaticKey "f"])
    Nothing
  assertParseText "a.e . d    or null" $ Fix $ NSelect (mkSym "a")
    (StaticKey "e" :| [StaticKey "d"])
    (Just mkNull)
  assertParseText "{}.\"\"or null" $ Fix $ NSelect (Fix (NSet []))
    (DynamicKey (Plain (DoubleQuoted [])) :| []) (Just mkNull)
  assertParseText "{ a = [1]; }.a or [2] ++ [3]" $ Fix $ NBinary NConcat
      (Fix (NSelect
                (Fix (NSet [NamedVar (StaticKey "a" :| [])
                                     (Fix (NList [Fix (NConstant (NInt 1))]))
                                     nullPos]))
                (StaticKey "a" :| [])
                (Just (Fix (NList [Fix (NConstant (NInt 2))])))))
      (Fix (NList [Fix (NConstant (NInt 3))]))

case_select_path = do
  assertParseText "f ./." $ Fix $ NBinary NApp (mkSym "f") (mkPath False "./.")
  assertParseText "f.b ../a" $ Fix $ NBinary NApp select (mkPath False "../a")
  assertParseText "{}./def" $ Fix $ NBinary NApp (Fix (NSet [])) (mkPath False "./def")
  assertParseText "{}.\"\"./def" $ Fix $ NBinary NApp
    (Fix $ NSelect (Fix (NSet [])) (DynamicKey (Plain (DoubleQuoted [])) :| []) Nothing)
    (mkPath False "./def")
 where select = Fix $ NSelect (mkSym "f") (mkSelector "b") Nothing

case_select_keyword = do
  assertParseText "{ false = \"foo\"; }" $ Fix $ NSet [NamedVar (mkSelector "false") (mkStr "foo") nullPos]

case_fun_app = do
  assertParseText "f a b" $ Fix $ NBinary NApp (Fix $ NBinary NApp (mkSym "f") (mkSym "a")) (mkSym "b")
  assertParseText "f a.x or null" $ Fix $ NBinary NApp (mkSym "f") $ Fix $
    NSelect (mkSym "a") (mkSelector "x") (Just mkNull)
  assertParseFail "f if true then null else null"

case_indented_string = do
  assertParseText "''a''" $ mkIndentedStr 0 "a"
  assertParseText "''\n  foo\n  bar''" $ mkIndentedStr 2 "foo\nbar"
  assertParseText "''        ''" $ mkIndentedStr 0 ""
  assertParseText "'''''''" $ mkIndentedStr 0 "''"
  assertParseText "''   ${null}\n   a${null}''" $ Fix $ NStr $ Indented 3
    [ Antiquoted mkNull
    , Plain "\na"
    , Antiquoted mkNull
    ]
  assertParseFail "'''''"
  assertParseFail "''   '"

case_indented_string_escape = assertParseText
  "'' ''\\n ''\\t ''\\\\ ''${ \\ \\n ' ''' ''" $
  mkIndentedStr 1 "\n \t \\ ${ \\ \\n ' '' "

case_operator_fun_app = do
  assertParseText "a ++ b" $ mkOper2 NConcat (mkSym "a") (mkSym "b")
  assertParseText "a ++ f b" $ mkOper2 NConcat (mkSym "a") $ Fix $ NBinary NApp
    (mkSym "f") (mkSym "b")

case_operators = do
  assertParseText "1 + 2 - 3" $ mkOper2 NMinus
    (mkOper2 NPlus (mkInt 1) (mkInt 2)) (mkInt 3)
  assertParseFail "1 + if true then 1 else 2"
  assertParseText "1 + (if true then 2 else 3)" $ mkOper2 NPlus (mkInt 1) $ Fix $ NIf
   (mkBool True) (mkInt 2) (mkInt 3)
  assertParseText "{ a = 3; } // rec { b = 4; }" $ mkOper2 NUpdate
    (Fix $ NSet [NamedVar (mkSelector "a") (mkInt 3) nullPos])
    (Fix $ NRecSet [NamedVar (mkSelector "b") (mkInt 4) nullPos])
  assertParseText "--a" $ mkOper NNeg $ mkOper NNeg $ mkSym "a"
  assertParseText "a - b - c" $ mkOper2 NMinus
    (mkOper2 NMinus (mkSym "a") (mkSym "b")) $
    mkSym "c"
  assertParseText "foo<bar" $ mkOper2 NLt (mkSym "foo") (mkSym "bar")
  assertParseFail "+ 3"
  assertParseFail "foo +"

case_comments = do
  Success expected <- parseNixFile "data/let.nix"
  assertParseFile "let-comments-multiline.nix" expected
  assertParseFile "let-comments.nix" expected

case_select_or_precedence =
    assertParsePrint [i|let
  matchDef = def:   matcher:
                      v:   let
                             case = builtins.head (builtins.attrNames v);
                           in (matcher.case or def case) (v.case);
in null|] [i|let
  matchDef = def:
    matcher:
      v:
        let
          case = builtins.head (builtins.attrNames v);
        in (matcher.case or def) case (v.case);
in null|]

case_select_or_precedence2 =
    assertParsePrint [i|let
  matchDef = def:   matcher:
                      v:   let
                             case = builtins.head (builtins.attrNames v);
                           in (matcher.case or null.foo) (v.case);
in null|] [i|let
  matchDef = def:
    matcher:
      v:
        let
          case = builtins.head (builtins.attrNames v);
        in (matcher.case or null).foo (v.case);
in null|]

tests :: TestTree
tests = $testGroupGenerator

--------------------------------------------------------------------------------

assertParseText :: Text -> NExpr -> Assertion
assertParseText str expected = case parseNixText str of
  Success actual ->
      assertEqual ("When parsing " ++ unpack str)
          (stripPositionInfo expected) (stripPositionInfo actual)
  Failure err    ->
      assertFailure $ "Unexpected error parsing `" ++ unpack str ++ "':\n" ++ show err

assertParseFile :: FilePath -> NExpr -> Assertion
assertParseFile file expected = do
  res <- parseNixFile $ "data/" ++ file
  case res of
    Success actual -> assertEqual ("Parsing data file " ++ file)
          (stripPositionInfo expected) (stripPositionInfo actual)
    Failure err    ->
        assertFailure $ "Unexpected error parsing data file `"
            ++ file ++ "':\n" ++ show err

assertParseFail :: Text -> Assertion
assertParseFail str = case parseNixText str of
  Failure _ -> return ()
  Success r ->
      assertFailure $ "Unexpected success parsing `"
          ++ unpack str ++ ":\nParsed value: " ++ show r

-- assertRoundTrip :: Text -> Assertion
-- assertRoundTrip src = assertParsePrint src src

assertParsePrint :: Text -> Text -> Assertion
assertParsePrint src expect =
  let Success expr = parseNixTextLoc src
      result = displayS
             . renderPretty 0.4 80
             . prettyNix
             . stripAnnotation
             $ expr
  in assertEqual "" expect (pack (result ""))
