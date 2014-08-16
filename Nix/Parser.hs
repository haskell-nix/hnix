{-# LANGUAGE CPP #-}

module Nix.Parser (parseNixFile, parseNixString, Result(..)) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Foldable
import qualified Data.Map as Map
import           Data.Text hiding (head, map, foldl1', foldl')
import           Nix.Types
import           Nix.Parser.Library
import           Prelude hiding (elem)

-- | The lexer for this parser is defined in 'Nix.Parser.Library'.
nixApp :: Parser NExpr
nixApp = go <$> some (whiteSpace *> nixExpr)
  where
    go []     = error "some has failed us"
    go [x]    = x
    go (f:x:xs) = go (Fix (NApp f x) : xs)

nixExpr :: Parser NExpr
nixExpr = nixExprWith nixOperators

nixExprWith :: [Either NSpecialOp [NOperatorDef]] -> Parser NExpr
nixExprWith = foldl' makeParser nixTerm
  where
    makeParser term (Left NSelectOp) = nixSelect term
    makeParser term (Left NAppOp) = term
    makeParser term (Left NHasAttrOp) = nixHasAttr term
    makeParser term (Right ops) = buildExpressionParser [map buildOp ops] term

    buildOp (NUnaryDef n op) = Prefix $ Fix . NOper . NUnary op <$ reservedOp n
    buildOp (NBinaryDef n op a) = Infix (mkOper <$ reservedOp n) (toAssoc a)
      where mkOper r1 = Fix . NOper . NBinary op r1

    toAssoc NAssocNone = AssocNone
    toAssoc NAssocLeft = AssocLeft
    toAssoc NAssocRight = AssocRight

nixAntiquoted :: Parser a -> Parser (Antiquoted a NExpr)
nixAntiquoted p = Plain <$> p
 <|> Antiquoted <$> (try (string "${") *> whiteSpace *> nixApp <* symbolic '}')

selDot :: Parser ()
selDot = try (char '.' *> notFollowedBy (("path" :: String) <$ nixPath)) *> whiteSpace

nixSelector :: Parser (NSelector NExpr)
nixSelector = keyName `sepBy1` selDot where

nixSelect :: Parser NExpr -> Parser NExpr
nixSelect term = build
  <$> term
  <*> optional ((,) <$> (selDot *> nixSelector) <*> optional (reserved "or" *> nixApp))
 where
  build t Nothing = t
  build t (Just (s,o)) = Fix $ NSelect t s o

nixHasAttr :: Parser NExpr -> Parser NExpr
nixHasAttr term = build <$> term <*> optional (reservedOp "?" *> nixSelector) where
  build t Nothing = t
  build t (Just s) = Fix $ NHasAttr t s

nixTerm :: Parser NExpr
nixTerm = choice
    [ nixInt
    , nixParens
    , nixList
    , nixLet
    , nixIf
    , nixBool
    , nixNull
    , nixPath                 -- can be expensive due to back-tracking
    , nixLambda <|> nixSet
    , nixStringExpr
    , nixSym
    ] <* whiteSpace

nixSym :: Parser NExpr
nixSym = mkSym <$> identifier

nixInt :: Parser NExpr
nixInt = mkInt <$> decimal <?> "integer"

nixBool :: Parser NExpr
nixBool = try (true <|> false) <?> "bool" where
  true = mkBool True <$ string "true"
  false = mkBool False <$ string "false"

nixNull :: Parser NExpr
nixNull = try (mkNull <$ string "null") <?> "null"

nixParens :: Parser NExpr
nixParens = parens nixApp <?> "parens"

nixList :: Parser NExpr
nixList = brackets (Fix . NList <$> many (listTerm <* whiteSpace)) <?> "list" where
 listTerm = nixSelect $ choice
   [ nixInt, nixParens, nixList, nixSet, nixBool, nixNull, nixPath, nixStringExpr
   , nixSym ]

nixPath :: Parser NExpr
nixPath = fmap mkPath $ (++)
  <$> try ((++) <$> many (oneOf pathChars) <*> string "/")
  <*> some (oneOf ('/':pathChars))
 where pathChars = ['A'..'Z'] ++ ['a'..'z'] ++ "._-+" ++ ['0'..'9']

nixLet :: Parser NExpr
nixLet =  fmap Fix $ NLet
      <$> (reserved "let" *> nixBinders)
      <*> (whiteSpace *> reserved "in" *> nixApp)

nixIf :: Parser NExpr
nixIf =  fmap Fix $ NIf
     <$> (reserved "if" *> nixApp)
     <*> (whiteSpace *> reserved "then" *> nixApp)
     <*> (whiteSpace *> reserved "else" *> nixApp)

nixLambda :: Parser NExpr
nixLambda = Fix <$> (NAbs <$> (try argExpr <?> "arguments") <*> nixApp)

nixStringExpr :: Parser NExpr
nixStringExpr = Fix . NStr <$> nixString

nixString :: Parser (NString NExpr)
nixString = NString . merge <$> (char '"' *> manyTill stringChar (symbolic '"'))
  where
    merge [] = [Plain ""]
    merge [x] = [x]
    merge (Plain a : Plain b : rs) = merge (Plain (a `append` b) : rs)
    merge (x : rs) = x : merge rs

    stringChar =  char '\\' *> (Plain . singleton <$> escapeCode)
              <|> Antiquoted <$> (try (string "${") *> nixApp <* char '}')
              <|> Plain . singleton <$> char '$'
              <|> Plain . pack <$> some (noneOf "\"\\$")

    escapeCode = choice $ map (\(x,y) -> x <$ char y)
      [ ('\n', 'n')
      , ('\r', 'r')
      , ('\t', 't')
      , ('\\', '\\')
      , ('$' , '$')
      , ('"' , '"')
      , ('\'', '\'')
      ]

argExpr :: Parser (Formals NExpr)
argExpr = choice
  [ idOrAtPattern <$> identifier <* whiteSpace <*> optional (symbolic '@' *> paramSet)
  , setOrAtPattern <$> paramSet <* whiteSpace <*> optional (symbolic '@' *> identifier)
  ] <* symbolic ':'
 where
  paramSet :: Parser (FormalParamSet NExpr)
  paramSet = FormalParamSet . Map.fromList <$> argList

  argList :: Parser [(Text, Maybe NExpr)]
  argList = braces ((argName <* whiteSpace) `sepBy` symbolic ',') <?> "arglist"

  argName :: Parser (Text, Maybe NExpr)
  argName = (,) <$> identifier <* whiteSpace
                <*> optional (symbolic '?' *> nixApp)

  idOrAtPattern :: Text -> Maybe (FormalParamSet NExpr) -> Formals NExpr
  idOrAtPattern i Nothing = FormalName i
  idOrAtPattern i (Just s) = FormalLeftAt i s

  setOrAtPattern :: FormalParamSet NExpr -> Maybe Text -> Formals NExpr
  setOrAtPattern s Nothing = FormalSet s
  setOrAtPattern s (Just i) = FormalRightAt s i

nixBinders :: Parser [Binding NExpr]
nixBinders = (inherit <|> namedVar) `endBy` symbolic ';' where
  inherit = Inherit <$> (reserved "inherit" *> optional scope) <*> many ((:[]) <$> keyName)
         <?> "inherited binding"
  namedVar = NamedVar <$> nixSelector <*> (symbolic '=' *> nixApp)
          <?> "variable binding"
  scope = parens nixApp <?> "inherit scope"

keyName :: Parser (NKeyName NExpr)
keyName = dynamicKey <|> staticKey where
  staticKey = StaticKey <$> identifier
  dynamicKey = DynamicKey <$> nixAntiquoted nixString

nixSet :: Parser NExpr
nixSet = Fix <$> (NSet <$> isRec <*> (braces nixBinders <?> "set")) where
  isRec = try (reserved "rec" *> pure Rec) <|> pure NonRec

parseNixFile :: MonadIO m => FilePath -> m (Result NExpr)
parseNixFile = parseFromFileEx $ nixApp <* eof

parseNixString :: String -> Result NExpr
parseNixString = parseFromString $ nixApp <* eof
