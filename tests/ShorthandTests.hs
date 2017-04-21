{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module ShorthandTests (tests) where

import Prelude

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import Control.Monad (forM_)
import Data.Monoid ((<>))
import Data.Fix

import Nix.Expr


case_mkDotsSymbolEscaping :: Assertion
case_mkDotsSymbolEscaping = do
  let check xs errmsg assert =
        forM_ xs $ \x -> assertBool (errmsg <> ": " <> show x) $ assert x
  check plain "not a plain value" $ assertIsSingle
  check nonPlain "not a non-plain value" $ not . assertIsSingle
  where
    plain = [ "abc09", "_A_'-", "AbC" ]
    nonPlain = [ "abc def", "\\", "'abc", "\"", "-foo", "a.b.c" ]
    assertIsSingle = isPlainSingle . getKey . mkDot "dummy"
    getKey (Fix (NSelect _ [key] _)) = key
    getKey _ = error "invalid"
    isPlainSingle (StaticKey _) = True
    isPlainSingle (DynamicKey (Plain (DoubleQuoted [Plain _]))) = False
    isPlainSingle _ = error "invalid"


---------------------------

tests :: TestTree
tests = $(testGroupGenerator)


