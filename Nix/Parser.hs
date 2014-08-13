{-# LANGUAGE CPP #-}

module Nix.Parser (parseNixFile, parseNixString, Result(..)) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Foldable
import           Data.List (foldl1')
import qualified Data.Map as Map
import           Data.Text hiding (head, map, foldl1')
import           Nix.Types
import           Nix.Parser.Library
import           Prelude hiding (elem)

-- | The lexer for this parser is defined in 'Nix.Parser.Library'.
nixApp :: Parser NExpr
nixApp = go <$> someTill (whiteSpace *> nixExpr True) (try (lookAhead stop))
  where
    go []     = error "some has failed us"
    go [x]    = x
    go (f:x:xs) = go (Fix (NApp f x) : xs)

    stop = () <$ oneOf "=,;])}" <|> stopWords <|> eof

nixExpr :: Bool -> Parser NExpr
nixExpr = buildExpressionParser table . nixTermOrAttr
  where
    table =
        [ [ prefix "-"  NNeg ]
        -- , [ prefix "~"  NSubpath ]  -- deprecated
        , [ binary "?"  NHasAttr AssocNone ]
        , [ binary "++" NConcat  AssocRight ]
        , [ binary "*"  NMult    AssocLeft, binary "/"  NDiv    AssocLeft ]
        , [ binary "+"  NPlus    AssocLeft, binary "-"  NMinus  AssocLeft ]
        , [ prefix "!"  NNot ]
        , [ binary "//" NUpdate  AssocRight ]
        , [ binary "<"  NLt      AssocLeft, binary ">"  NGt     AssocLeft
          , binary "<=" NLte     AssocLeft, binary ">=" NGte    AssocLeft ]
        , [ binary "==" NEq      AssocNone, binary "!=" NNEq    AssocNone ]
        , [ binary "&&" NAnd     AssocLeft ]
        , [ binary "||" NOr      AssocLeft ]
        , [ binary "->" NImpl    AssocNone ]
        ]

    binary  name fun =
        Infix  $ (\x y -> Fix (NOper (fun x y))) <$ reservedOp name
    prefix  name fun =
        Prefix $ Fix . NOper . fun <$ reservedOp name
    -- postfix name fun = Postfix (Fix . NOper . fun <$ symbol name)

nixTermOrAttr :: Bool -> Parser NExpr
nixTermOrAttr = buildExpressionParser table . nixTerm where
  table = [[Infix ((\x y -> Fix (NOper (NAttr x y))) <$ reservedOp ".") AssocLeft]]

nixTerm :: Bool -> Parser NExpr
nixTerm allowLambdas = choice
    [ nixInt
    , nixParens
    , nixList
    , nixLet
    , nixIf
    , nixBool
    , nixNull
    , nixPath                   -- can be expensive due to back-tracking
    , setLambdaStringOrSym allowLambdas
    ]

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
nixList = brackets (Fix . NList <$> many (nixTermOrAttr False <* whiteSpace)) <?> "list"

nixPath :: Parser NExpr
nixPath = try $ fmap mkPath $ mfilter ('/' `elem`) $ some (oneOf "A-Za-z_0-9.:/")

nixLet :: Parser NExpr
nixLet =  fmap Fix $ NLet
      <$> (reserved "let" *> nixBinders)
      <*> (whiteSpace *> reserved "in" *> nixApp)

nixIf :: Parser NExpr
nixIf =  fmap Fix $ NIf
     <$> (reserved "if" *> nixApp)
     <*> (whiteSpace *> reserved "then" *> nixApp)
     <*> (whiteSpace *> reserved "else" *> nixApp)

-- | This is a bit tricky because we don't know whether we're looking at a set
--   or a lambda until we've looked ahead a bit.  And then it may be neither,
--   in which case we fall back to expected a plain string or identifier.
setLambdaStringOrSym :: Bool -> Parser NExpr
setLambdaStringOrSym True = try nixLambda <|> setLambdaStringOrSym False
setLambdaStringOrSym False  = try nixSet <|> keyName

nixLambda :: Parser NExpr
nixLambda = Fix <$> (NAbs <$> (argExpr <?> "arguments") <*> nixApp)

stringish :: Parser NExpr
stringish =  (char '"' *> (merge <$> manyTill stringChar (char '"')))
         <|> (char '$' *> braces nixApp)
  where
    merge = foldl1' (\x y -> Fix (NOper (NConcat x y)))

    stringChar =  char '\\' *> (mkStr . singleton <$> anyChar)
              <|> (try (string "${") *> nixApp <* char '}')
              <|> (mkStr . pack <$> many (noneOf "\"\\"))

argExpr :: Parser NExpr
argExpr = (try (Fix . NArgs . FormalSet <$> paramSet)
          <|> try (Fix . NArgs . FormalName <$> identifier <* whiteSpace)
          <|> try (Fix . NArgs <$> (FormalLeftAt <$> identifier <* whiteSpace <*> paramSet))
          <|> try (Fix . NArgs <$> (FormalRightAt <$> paramSet <*> identifier <* whiteSpace))) <* symbolic ':'
  where
  paramSet :: Parser (FormalParamSet NExpr)
  paramSet =  (FormalParamSet . Map.fromList <$> argList)
  argList :: Parser [(Text, Maybe NExpr)]
  argList =  braces ((argName <* whiteSpace) `sepBy` symbolic ',') <* symbolic ':'
           <?> "arglist"
  argName :: Parser (Text, Maybe NExpr)
  argName = (,) <$> (identifier <* whiteSpace)
                  <*> optional (symbolic '?' *> nixExpr False)

nixBinders :: Parser [Binding NExpr]
nixBinders = choice
  [ reserved "inherit" *> whiteSpace *> (scopedInherit <|> inherit) <?> "inherited binding"
  , namedVar
  ] `endBy` symbolic ';'
 where
  scopedInherit = try (symbolic '(') *>
    (ScopedInherit <$> nixExpr False <* symbolic ')' <*> many keyName) <?> "scoped inherit binding"
  inherit = Inherit <$> many keyName
  namedVar =  NamedVar <$> keyName <*> (symbolic '=' *> nixApp) <?> "variable binding"

keyName :: Parser NExpr
keyName = (stringish <|> (mkSym <$> identifier)) <* whiteSpace

nixSet :: Parser NExpr
nixSet = Fix <$> (NSet <$> isRec <*> (braces nixBinders <?> "set")) where
  isRec = try (reserved "rec" *> pure Rec) <|> pure NonRec

parseNixFile :: MonadIO m => FilePath -> m (Result NExpr)
parseNixFile = parseFromFileEx $ nixApp <* eof

parseNixString :: String -> Result NExpr
parseNixString = parseFromString $ nixApp <* eof
