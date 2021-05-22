{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}


module ParserTests (tests) where

import Data.Fix
import NeatInterpolation (text)
import Nix.Atoms
import Nix.Expr
import Nix.Parser
import Nix.Pretty
import Prettyprinter
import Prettyprinter.Render.Text
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

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
  assertParseText "{ a = 23; b = 4; }" $ Fix $ NSet NNonRecursive
    [ NamedVar (mkSelector "a") (mkInt 23) nullPos
    , NamedVar (mkSelector "b") (mkInt 4) nullPos
    ]
  assertParseFail "{ a = 23 }"

case_set_inherit = do
  assertParseText "{ e = 3; inherit a b; }" $ Fix $ NSet NNonRecursive
    [ NamedVar (mkSelector "e") (mkInt 3) nullPos
    , Inherit Nothing (StaticKey <$> ["a", "b"]) nullPos
    ]
  assertParseText "{ inherit; }" $ Fix $ NSet NNonRecursive [ Inherit Nothing mempty nullPos ]

case_set_scoped_inherit = assertParseText "{ inherit (a) b c; e = 4; inherit(a)b c; }" $ Fix $ NSet NNonRecursive
  [ Inherit (pure (mkSym "a")) (StaticKey <$> ["b", "c"]) nullPos
  , NamedVar (mkSelector "e") (mkInt 4) nullPos
  , Inherit (pure (mkSym "a")) (StaticKey <$> ["b", "c"]) nullPos
  ]

case_set_rec = assertParseText "rec { a = 3; b = a; }" $ Fix $ NSet NRecursive
  [ NamedVar (mkSelector "a") (mkInt 3) nullPos
  , NamedVar (mkSelector "b") (mkSym "a") nullPos
  ]

case_set_complex_keynames = do
  assertParseText "{ \"\" = null; }" $ Fix $ NSet NNonRecursive
    [ NamedVar (DynamicKey (Plain (DoubleQuoted mempty)) :| mempty) mkNull nullPos ]
  assertParseText "{ a.b = 3; a.c = 4; }" $ Fix $ NSet NNonRecursive
    [ NamedVar (StaticKey "a" :| [StaticKey "b"]) (mkInt 3) nullPos
    , NamedVar (StaticKey "a" :| [StaticKey "c"]) (mkInt 4) nullPos
    ]
  assertParseText "{ ${let a = \"b\"; in a} = 4; }" $ Fix $ NSet NNonRecursive
    [ NamedVar (DynamicKey (Antiquoted letExpr) :| mempty) (mkInt 4) nullPos ]
  assertParseText "{ \"a${let a = \"b\"; in a}c\".e = 4; }" $ Fix $ NSet NNonRecursive
    [ NamedVar (DynamicKey (Plain str) :| [StaticKey "e"]) (mkInt 4) nullPos ]
 where
  letExpr = Fix $ NLet [NamedVar (mkSelector "a") (mkStr "b") nullPos] (mkSym "a")
  str = DoubleQuoted [Plain "a", Antiquoted letExpr, Plain "c"]

case_set_inherit_direct = assertParseText "{ inherit ({a = 3;}); }" $ Fix $ NSet NNonRecursive
  [ Inherit (pure $ Fix $ NSet NNonRecursive [NamedVar (mkSelector "a") (mkInt 3) nullPos]) mempty nullPos
  ]

case_inherit_selector = do
  assertParseText "{ inherit \"a\"; }" $ Fix $ NSet NNonRecursive
    [Inherit Nothing [DynamicKey (Plain (DoubleQuoted [Plain "a"]))] nullPos]
  assertParseFail "{ inherit a.x; }"

case_int_list = assertParseText "[1 2 3]" $ Fix $ NList
  [ mkInt i | i <- [1,2,3] ]

case_int_null_list = assertParseText "[1 2 3 null 4]" $ Fix (NList (fmap (Fix . NConstant) [NInt 1, NInt 2, NInt 3, NNull, NInt 4]))

case_mixed_list = do
  assertParseText "[{a = 3;}.a (if true then null else false) null false 4 [] c.d or null]" $ Fix $ NList
    [ Fix (NSelect (Fix (NSet NNonRecursive [NamedVar (mkSelector "a") (mkInt 3) nullPos]))
                   (mkSelector "a") Nothing)
    , Fix (NIf (mkBool True) mkNull (mkBool False))
    , mkNull, mkBool False, mkInt 4, Fix (NList mempty)
    , Fix (NSelect (mkSym "c") (mkSelector "d") (pure mkNull))
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
  assertParseText "a:{}" $ Fix $ NAbs (Param "a") $ Fix $ NSet NNonRecursive mempty
  assertParseText "a:[a]" $ Fix $ NAbs (Param "a") $ Fix $ NList [mkSym "a"]
  assertParseFail "def:"

case_lambda_pattern = do
  assertParseText "{b, c ? 1}: b" $
    Fix $ NAbs (fixed args mempty) (mkSym "b")
  assertParseText "{ b ? x: x  }: b" $
    Fix $ NAbs (fixed args2 mempty) (mkSym "b")
  assertParseText "a@{b,c ? 1}: b" $
    Fix $ NAbs (fixed args (pure "a")) (mkSym "b")
  assertParseText "{b,c?1}@a: c" $
    Fix $ NAbs (fixed args (pure "a")) (mkSym "c")
  assertParseText "{b,c?1,...}@a: c" $
    Fix $ NAbs (variadic vargs (pure "a")) (mkSym "c")
  assertParseText "{...}: 1" $
    Fix $ NAbs (variadic mempty mempty) (mkInt 1)
  assertParseFail "a@b: a"
  assertParseFail "{a}@{b}: a"
 where
  fixed args = ParamSet args False
  variadic args = ParamSet args True
  args = [("b", Nothing), ("c", pure $ mkInt 1)]
  vargs = [("b", Nothing), ("c", pure $ mkInt 1)]
  args2 = [("b", pure lam)]
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
    aset = Fix $ NSet NRecursive [NamedVar (mkSelector "body") (mkInt 1) nullPos]

case_nested_let = do
  assertParseText "let a = 4; in let b = 5; in a" $ Fix $ NLet
    [NamedVar (mkSelector "a") (mkInt 4) nullPos]
    (Fix $ NLet [NamedVar (mkSelector "b") (mkInt 5) nullPos] $ mkSym "a")
  assertParseFail "let a = 4; let b = 3; in b"

case_let_scoped_inherit = do
  assertParseText "let a = null; inherit (b) c; in c" $ Fix $ NLet
    [ NamedVar (mkSelector "a") mkNull nullPos
    , Inherit (pure $ mkSym "b") [StaticKey "c"] nullPos ]
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

case_simple_string = traverse_ makeTextParseTest ["abcdef", "a", "A", "   a a  ", ""]

case_string_dollar = traverse_ makeTextParseTest ["a$b", "a$$b", "$cdef", "gh$i"]

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
    (pure mkNull)
  assertParseText "{}.\"\"or null" $ Fix $ NSelect (Fix (NSet NNonRecursive mempty))
    (DynamicKey (Plain (DoubleQuoted mempty)) :| mempty) (pure mkNull)
  assertParseText "{ a = [1]; }.a or [2] ++ [3]" $ Fix $ NBinary NConcat
      (Fix (NSelect
                (Fix (NSet NNonRecursive [NamedVar (StaticKey "a" :| mempty)
                                     (Fix (NList [Fix (NConstant (NInt 1))]))
                                     nullPos]))
                (StaticKey "a" :| mempty)
                (pure (Fix (NList [Fix (NConstant (NInt 2))])))))
      (Fix (NList [Fix (NConstant (NInt 3))]))

case_select_path = do
  assertParseText "f ./." $ Fix $ NBinary NApp (mkSym "f") (mkPath False "./.")
  assertParseText "f.b ../a" $ Fix $ NBinary NApp select (mkPath False "../a")
  assertParseText "{}./def" $ Fix $ NBinary NApp (Fix (NSet NNonRecursive mempty)) (mkPath False "./def")
  assertParseText "{}.\"\"./def" $ Fix $ NBinary NApp
    (Fix $ NSelect (Fix (NSet NNonRecursive mempty)) (DynamicKey (Plain (DoubleQuoted mempty)) :| mempty) Nothing)
    (mkPath False "./def")
 where select = Fix $ NSelect (mkSym "f") (mkSelector "b") Nothing

case_select_keyword = do
  assertParseText "{ false = \"foo\"; }" $ Fix $ NSet NNonRecursive [NamedVar (mkSelector "false") (mkStr "foo") nullPos]

case_fun_app = do
  assertParseText "f a b" $ Fix $ NBinary NApp (Fix $ NBinary NApp (mkSym "f") (mkSym "a")) (mkSym "b")
  assertParseText "f a.x or null" $ Fix $ NBinary NApp (mkSym "f") $ Fix $
    NSelect (mkSym "a") (mkSelector "x") (pure mkNull)
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
    (Fix $ NSet NNonRecursive [NamedVar (mkSelector "a") (mkInt 3) nullPos])
    (Fix $ NSet NRecursive [NamedVar (mkSelector "b") (mkInt 4) nullPos])
  assertParseText "--a" $ mkOper NNeg $ mkOper NNeg $ mkSym "a"
  assertParseText "a - b - c" $ mkOper2 NMinus
    (mkOper2 NMinus (mkSym "a") (mkSym "b")) $
    mkSym "c"
  assertParseText "foo<bar" $ mkOper2 NLt (mkSym "foo") (mkSym "bar")
  assertParseFail "+ 3"
  assertParseFail "foo +"

case_comments =
  do
    Right expected <- parseNixFile "data/let.nix"
    assertParseFile "let-comments-multiline.nix" expected
    assertParseFile "let-comments.nix" expected

case_select_or_precedence =
    assertParsePrint [text|let
  matchDef = def:   matcher:
                      v:   let
                             case = builtins.head (builtins.attrNames v);
                           in (matcher.case or def case) (v.case);
in null|] [text|let
  matchDef = def:
    matcher:
      v:
        let
          case = builtins.head (builtins.attrNames v);
        in (matcher.case or def) case (v.case);
in null|]

case_select_or_precedence2 =
    assertParsePrint [text|let
  matchDef = def:   matcher:
                      v:   let
                             case = builtins.head (builtins.attrNames v);
                           in (matcher.case or null.foo) (v.case);
in null|] [text|let
  matchDef = def:
    matcher:
      v:
        let
          case = builtins.head (builtins.attrNames v);
        in (matcher.case or null).foo (v.case);
in null|]

case_simpleLoc =
  let
    mkSPos l c = SourcePos "<string>" (mkPos l) (mkPos c)
    mkSpan l1 c1 l2 c2 = SrcSpan (mkSPos l1 c1) (mkSPos l2 c2)
  in
    assertParseTextLoc [text|let
    foo = bar
         baz "qux";
    in foo
    |]
    (Fix
      (NLet_
        (mkSpan 1 1 4 7)
        [ NamedVar
            (StaticKey "foo" :| [])
            (Fix
              (NBinary_
                (mkSpan 2 7 3 15)
                NApp
                (Fix
                  (NBinary_ (mkSpan 2 7 3 9)
                            NApp
                            (Fix (NSym_ (mkSpan 2 7 2 10) "bar"))
                            (Fix (NSym_ (mkSpan 3 6 3 9) "baz"))
                  )
                )
                (Fix (NStr_ (mkSpan 3 10 3 15) (DoubleQuoted [Plain "qux"])))
              )
            )
            (mkSPos 2 1)
        ]
        (Fix (NSym_ (mkSpan 4 4 4 7) "foo"))
      )
    )


tests :: TestTree
tests = $testGroupGenerator

---------------------------------------------------------------------------------

assertParseText :: Text -> NExpr -> Assertion
assertParseText str expected =
  either
    (\ err ->
      assertFailure $ toString $ "Unexpected fail parsing `" <> str <> "':\n" <> show err
    )
    (assertEqual
      ("When parsing " <> toString str)
      (stripPositionInfo expected)
      . stripPositionInfo
    )
    (parseNixText str)

assertParseTextLoc :: Text -> NExprLoc -> Assertion
assertParseTextLoc str expected =
  either
    (\ err ->
      assertFailure $ toString $ "Unexpected fail parsing `" <> str <> "':\n" <> show err
    )
    (assertEqual
      ("When parsing " <> toString str)
      expected
    )
    (parseNixTextLoc str)

assertParseFile :: FilePath -> NExpr -> Assertion
assertParseFile file expected =
  do
  res <- parseNixFile $ "data/" <> file
  either
    (\ err ->
      assertFailure $ "Unexpected fail parsing data file `" <> file <> "':\n" <> show err
    )
    (assertEqual
      ("Parsing data file " <> file)
      (stripPositionInfo expected)
      . stripPositionInfo
    )
    res

assertParseFail :: Text -> Assertion
assertParseFail str =
  either
    (const pass)
    (\ r ->
      assertFailure $ toString $ "Unexpected success parsing `" <> str <> ":\nParsed value: " <> show r
    )
    (parseNixText str)

-- assertRoundTrip :: Text -> Assertion
-- assertRoundTrip src = assertParsePrint src src

assertParsePrint :: Text -> Text -> Assertion
assertParsePrint src expect =
  let
    Right expr = parseNixTextLoc src
    result =
      renderStrict
      . layoutPretty (LayoutOptions $ AvailablePerLine 80 0.4)
      . prettyNix
      . stripAnnotation $
        expr
  in assertEqual "" expect result
