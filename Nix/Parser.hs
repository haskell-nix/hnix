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
nixApp = foldl' go <$> (whiteSpace *> nixExpr) <*> many nixFunArg
  where go f a = Fix (NApp f a)

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

antiStart :: Parser String
antiStart = try (string "${") <?> show ("${" :: String)

nixAntiquoted :: Parser a -> Parser (Antiquoted a NExpr)
nixAntiquoted p = Antiquoted <$> (antiStart *> nixApp <* symbolic '}') <|> Plain <$> p

selDot :: Parser ()
selDot = try (char '.' *> notFollowedBy (("path" :: String) <$ nixPath)) *> whiteSpace
      <?> "."

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
    , nixAssert
    , nixWith
    , nixBool
    , nixNull
    , nixPath
    , nixLambda
    , nixSet
    , nixStringExpr
    , nixSym
    ] <* whiteSpace

nixSym :: Parser NExpr
nixSym = mkSym <$> identifier

nixInt :: Parser NExpr
nixInt = mkInt <$> token decimal <?> "integer"

nixBool :: Parser NExpr
nixBool = try (true <|> false) <?> "bool" where
  true = mkBool True <$ symbol "true"
  false = mkBool False <$ symbol "false"

nixNull :: Parser NExpr
nixNull = mkNull <$ try (symbol "null") <?> "null"

nixParens :: Parser NExpr
nixParens = parens nixApp <?> "parens"

nixFunArg :: Parser NExpr
nixFunArg = nixSelect $ choice
  [ nixInt, nixParens, nixList, nixSet, nixBool, nixNull, nixPath, nixStringExpr
  , nixSym ]

nixList :: Parser NExpr
nixList = brackets (Fix . NList <$> many nixFunArg) <?> "list"

nixPath :: Parser NExpr
nixPath = token $ fmap mkPath $ (++)
  <$> (try ((++) <$> many (oneOf pathChars) <*> string "/") <?> "path")
  <*> some (oneOf ('/':pathChars))
  <?> "path"
 where pathChars = ['A'..'Z'] ++ ['a'..'z'] ++ "._-+" ++ ['0'..'9']

nixLet :: Parser NExpr
nixLet =  fmap Fix $ NLet
      <$> (reserved "let" *> nixBinders)
      <*> (whiteSpace *> reserved "in" *> nixApp)
      <?> "let"

nixIf :: Parser NExpr
nixIf =  fmap Fix $ NIf
     <$> (reserved "if" *> nixApp)
     <*> (whiteSpace *> reserved "then" *> nixApp)
     <*> (whiteSpace *> reserved "else" *> nixApp)
     <?> "if"

nixAssert :: Parser NExpr
nixAssert = fmap Fix $ NAssert
  <$> (reserved "assert" *> nixApp)
  <*> (semi *> nixApp)

nixWith :: Parser NExpr
nixWith = fmap Fix $ NWith
  <$> (reserved "with" *> nixApp)
  <*> (semi *> nixApp)

nixLambda :: Parser NExpr
nixLambda = Fix <$> (NAbs <$> (try argExpr <?> "lambda arguments") <*> nixApp) <?> "lambda"

nixStringExpr :: Parser NExpr
nixStringExpr = Fix . NStr <$> nixString

nixString :: Parser (NString NExpr)
nixString = NString . merge <$> (char '"' *> manyTill stringChar (symbolic '"')) <?> "string"
  where
    merge [] = [Plain ""]
    merge [x] = [x]
    merge (Plain a : Plain b : rs) = merge (Plain (a `append` b) : rs)
    merge (x : rs) = x : merge rs

    stringChar =  char '\\' *> (Plain . singleton <$> escapeCode)
      <|> Antiquoted <$> (antiStart *> nixApp <* char '}') -- don't skip trailing space
      <|> Plain . singleton <$> char '$'
      <|> Plain . pack <$> some (noneOf "\"\\$")

    escapeCode = choice [ c <$ char e | (c,e) <- escapeCodes ]

argExpr :: Parser (Formals NExpr)
argExpr = choice
  [ idOrAtPattern <$> identifier <*> optional (symbolic '@' *> paramSet)
  , setOrAtPattern <$> paramSet <*> optional (symbolic '@' *> identifier)
  ] <* symbolic ':'
 where
  paramSet :: Parser (FormalParamSet NExpr)
  paramSet = FormalParamSet . Map.fromList <$> argList

  argList :: Parser [(Text, Maybe NExpr)]
  argList = braces (argName `sepBy` symbolic ',') <?> "arglist"

  argName :: Parser (Text, Maybe NExpr)
  argName = (,) <$> identifier
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
nixSet = Fix <$> (NSet <$> isRec <*> braces nixBinders) <?> "set" where
  isRec = (try (reserved "rec" *> pure Rec) <?> "recursive set")
       <|> pure NonRec

parseNixFile :: MonadIO m => FilePath -> m (Result NExpr)
parseNixFile = parseFromFileEx $ nixApp <* eof

parseNixString :: String -> Result NExpr
parseNixString = parseFromString $ nixApp <* eof
