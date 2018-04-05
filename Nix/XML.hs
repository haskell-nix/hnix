{-# LANGUAGE LambdaCase #-}

module Nix.XML where

import           Data.Fix
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as Text
import           Nix.Atoms
import           Nix.Monad
import           Text.XML.Light

toXML :: Functor m => NValueNF m -> String
toXML = (.) ((++ "\n") .
             ("<?xml version='1.0' encoding='utf-8'?>\n" ++) .
             ppElement .
             (\e -> Element (unqual "expr") [] [Elem e] Nothing))
        $ cata
        $ \case
    NVConstant a -> case a of
        NInt n   -> elem "int" "value" (show n)
        NFloat f -> elem "float" "value" (show f)
        NBool b  -> elem "bool" "value" (if b then "true" else "false")
        NNull    -> Element (unqual "null") [] [] Nothing
        NUri u   -> elem "uri" "value" (Text.unpack u)

    NVStr t _ -> elem "string" "value" (Text.unpack t)
    NVList l  -> Element (unqual "list") [] (Elem <$> l) Nothing

    NVSet s   -> Element (unqual "attrs") []
        (map (\(k, v) -> Elem (Element (unqual "attr")
                                      [Attr (unqual "name") (Text.unpack k)]
                                      [Elem v] Nothing))
             (M.toList s)) Nothing

    NVClosure _ _p _  ->
        Element (unqual "function") []
                (error "NYI: XML function param attrset") Nothing
    NVLiteralPath fp -> elem "path" "value" fp
    NVEnvPath p      -> elem "path" "value" p
    NVBuiltin name _ -> elem "function" "name" name
  where
    elem n a v = Element (unqual n) [Attr (unqual a) v] [] Nothing
