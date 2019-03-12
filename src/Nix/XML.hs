{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Nix.XML (toXML) where

import           Control.Monad.Free
import qualified Data.HashMap.Lazy as M
import           Data.List
import           Data.Ord
import qualified Data.Text as Text
import           Nix.Atoms
import           Nix.Expr.Types
import           Nix.String
import           Nix.Value
import           Text.XML.Light

{-
toXML :: Functor m => NValueNF m -> NixString
toXML = runWithStringContext . fmap pp . iterM phi . check
  where
    pp = ("<?xml version='1.0' encoding='utf-8'?>\n" <>)
      . (<> "\n")
      . Text.pack
      . ppElement
      . (\e -> Element (unqual "expr") [] [Elem e] Nothing)

    check :: NValueNF f -> Free (NValueF f) Element
    check = fmap $ const $ mkElem "cycle" "value" ""

    phi :: NValueF f (WithStringContext Element) -> WithStringContext Element
    phi = \case
        NVConstantF a -> case a of
            NInt n   -> return $ mkElem "int" "value" (show n)
            NFloat f -> return $ mkElem "float" "value" (show f)
            NBool b  -> return $ mkElem "bool" "value" (if b then "true" else "false")
            NNull    -> return $ Element (unqual "null") [] [] Nothing

        NVStrF str -> mkElem "string" "value" . Text.unpack <$> extractNixString str
        NVListF l  -> sequence l >>= \els ->
          return $ Element (unqual "list") [] (Elem <$> els) Nothing

        NVSetF s _ -> sequence s >>= \kvs ->
          return $ Element (unqual "attrs") []
            (map (\(k, v) ->
                      Elem (Element (unqual "attr")
                               [Attr (unqual "name") (Text.unpack k)]
                               [Elem v] Nothing))
                 (sortBy (comparing fst) $ M.toList kvs)) Nothing

        NVClosureF p _    -> return $ Element (unqual "function") [] (paramsXML p) Nothing
        NVPathF fp        -> return $ mkElem "path" "value" fp
        NVBuiltinF name _ -> return $ mkElem "function" "name" name
-}

mkElem :: String -> String -> String -> Element
mkElem n a v = Element (unqual n) [Attr (unqual a) v] [] Nothing

paramsXML :: Params r -> [Content]
paramsXML (Param name) =
    [Elem $ mkElem "varpat" "name" (Text.unpack name)]
paramsXML (ParamSet s b mname) =
    [Elem $ Element (unqual "attrspat") (battr <> nattr) (paramSetXML s) Nothing]
  where
    battr = [ Attr (unqual "ellipsis") "1" | b ]
    nattr = maybe [] ((:[]) . Attr (unqual "name") . Text.unpack) mname

paramSetXML :: ParamSet r -> [Content]
paramSetXML = map (\(k,_) -> Elem $ mkElem "attr" "name" (Text.unpack k))
