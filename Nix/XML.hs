{-# LANGUAGE LambdaCase #-}

module Nix.XML where

import           Data.Fix
import qualified Data.HashMap.Lazy as M
import qualified Data.HashMap.Strict.InsOrd as OM
import           Data.List
import           Data.Ord
import qualified Data.Text as Text
import           Nix.Atoms
import           Nix.Expr.Types
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
        NInt n   -> mkElem "int" "value" (show n)
        NFloat f -> mkElem "float" "value" (show f)
        NBool b  -> mkElem "bool" "value" (if b then "true" else "false")
        NNull    -> Element (unqual "null") [] [] Nothing
        NUri u   -> mkElem "uri" "value" (Text.unpack u)

    NVStr t _ -> mkElem "string" "value" (Text.unpack t)
    NVList l  -> Element (unqual "list") [] (Elem <$> l) Nothing

    NVSet s   -> Element (unqual "attrs") []
        (map (\(k, v) -> Elem (Element (unqual "attr")
                                      [Attr (unqual "name") (Text.unpack k)]
                                      [Elem v] Nothing))
             (sortBy (comparing fst) $ M.toList s)) Nothing

    NVClosure _ p _  ->
        Element (unqual "function") [] (paramsXML p) Nothing
    NVLiteralPath fp -> mkElem "path" "value" fp
    NVEnvPath p      -> mkElem "path" "value" p
    NVBuiltin name _ -> mkElem "function" "name" name

mkElem :: String -> String -> String -> Element
mkElem n a v = Element (unqual n) [Attr (unqual a) v] [] Nothing

paramsXML :: Params r -> [Content]
paramsXML (Param name) =
    [Elem $ mkElem "varpat" "name" (Text.unpack name)]
paramsXML (ParamSet s b mname) =
    [Elem $ Element (unqual "attrspat") (battr ++ nattr) (paramSetXML s) Nothing]
  where
    battr = if b then [Attr (unqual "ellipsis") "1"] else []
    nattr = maybe [] ((:[]) . Attr (unqual "name") . Text.unpack) (mname)

paramSetXML :: ParamSet r -> [Content]
paramSetXML m = map (\(k,_) -> Elem $ mkElem "attr" "name" (Text.unpack k)) $ OM.toList m
