
module Nix.XML
  ( toXML )
where

import qualified Data.HashMap.Lazy             as M
import           Nix.Atoms
import           Nix.Expr.Types
import           Nix.String
import           Nix.Value
import           Text.XML.Light                 ( Element(Element)
                                                , Attr(Attr)
                                                , Content(Elem)
                                                , unqual
                                                , ppElement
                                                )

toXML :: forall t f m . MonadDataContext f m => NValue t f m -> NixString
toXML = runWithStringContext . fmap pp . iterNValueByDiscardWith cyc phi
 where
  cyc = pure $ mkEVal "string" "<expr>"

  pp :: Element -> Text
  pp e =
    heading
    <> fromString
        (ppElement $
          mkE
            "expr"
            (one $ Elem e)
        )
    <> "\n"
   where
    heading = "<?xml version='1.0' encoding='utf-8'?>\n"

  phi :: NValue' t f m (WithStringContext Element) -> WithStringContext Element
  phi = \case
    NVConstant' a ->
      pure $
        case a of
          NURI   t -> mkEVal "string" t
          NInt   n -> mkEVal "int"    $ show n
          NFloat f -> mkEVal "float"  $ show f
          NBool  b -> mkEVal "bool"   $ if b then "true" else "false"
          NNull    -> mkE    "null"     mempty

    NVStr' str ->
      mkEVal "string" <$> extractNixString str
    NVList' l ->
      mkE "list" . fmap Elem <$> sequenceA l

    NVSet' _ s ->
      mkE
        "attrs"
        . fmap
            mkElem'
            . sortWith fst . M.toList
        <$> sequenceA s
     where
      mkElem' :: (VarName, Element) -> Content
      mkElem' (k, v) =
        Elem $
          Element
            (unqual "attr")
            (one $ Attr (unqual "name") $ toString k)
            (one $ Elem v)
            Nothing

    NVClosure' p _ ->
      pure $
        mkE
          "function"
          (paramsXML p)
    NVPath' fp        -> pure $ mkEVal "path" $ fromString $ coerce fp
    NVBuiltin' name _ -> pure $ mkEName "function" name

mkE :: Text -> [Content] -> Element
mkE (toString -> n) c =
  Element
    (unqual n)
    mempty
    c
    Nothing

mkElem :: Text -> Text -> Text -> Element
mkElem (toString -> n) (toString -> a) (toString -> v) =
  Element
    (unqual n)
    (one $ Attr (unqual a) v)
    mempty
    Nothing

mkEVal :: Text -> Text -> Element
mkEVal = (`mkElem` "value")

mkEName :: Text -> VarName -> Element
mkEName x (coerce -> y) = (`mkElem` "name") x y

paramsXML :: Params r -> [Content]
paramsXML (Param name) = one $ Elem $ mkEName "varpat" name
paramsXML (ParamSet mname variadic pset) =
  one $ Elem $ Element (unqual "attrspat") (battr <> nattr) (paramSetXML pset) Nothing
 where
  battr =
    one (Attr (unqual "ellipsis") "1") `whenTrue` (variadic == Variadic)
  nattr =
    (one . Attr (unqual "name") . toString) `whenJust` mname

paramSetXML :: ParamSet r -> [Content]
paramSetXML = fmap (Elem . mkEName "attr" . fst)
