--{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
--{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
--{-# LANGUAGE ViewPatterns #-}

module Nix where

import           Control.Applicative
import           Control.Monad hiding (forM_)
--import           Control.Monad.IO.Class
--import           Control.Monad.Trans.Class
--import           Control.Monad.Trans.Control
--import           Control.Monad.Trans.Either
import           Data.Char
import           Data.Data
import           Data.Foldable
import           Data.Map (Map)
import qualified Data.Map as Map
--import           Data.Monoid
import           Data.Text hiding (concat, concatMap, head, map)
import           Data.Text.IO
--import           Data.Traversable
--import           Data.Typeable
import           GHC.Generics
import qualified Prelude
import           Prelude hiding (readFile, concat, concatMap, elem)
import           System.Environment
import           System.IO.Memoize
-- import           Text.Parsec hiding ((<|>), many, optional)
-- import           Text.Parsec.Text
import           Text.Trifecta
import           Text.Parser.LookAhead

--import Debug.Trace
trace :: a -> b -> b
trace _ x = x

loeb :: Functor f => f (f a -> a) -> f a
loeb xs = ys where ys = fmap ($ ys) xs
{-# INLINE loeb #-}

data NAtom
    = NStr Text
    | NInt Integer
    | NPath FilePath
    | NBool Bool
    | NSym Text
    | NNull
    deriving (Eq, Ord, Generic, Typeable, Data)

instance Show (NAtom) where
    show (NStr s)   = show s
    show (NInt i)   = show i
    show (NPath p)  = show p
    show (NBool b)  = if b then "true" else "false"
    show (NSym s)   = unpack s
    show NNull      = "null"

atomText :: NAtom -> Text
atomText (NStr s)   = s
atomText (NInt i)   = pack (show i)
atomText (NPath p)  = pack p
atomText (NBool b)  = if b then "true" else "false"
atomText (NSym s)   = s
atomText NNull      = "null"

dumpAtom :: NAtom -> String
dumpAtom (NStr s)   = "NStr " ++ show s
dumpAtom (NInt i)   = "NInt " ++ show i
dumpAtom (NPath p)  = "NPath " ++ show p
dumpAtom (NBool b)  = "NBool " ++ show b
dumpAtom (NSym s)   = "NSym " ++ show s
dumpAtom NNull      = "NNull"

data NExprF r
    = NConstant NAtom

    | NList [r]
    | NConcat [r]
      -- ^ A "concat" is a list of things which must combine to form a string.
    | NArgSet (Map Text (Maybe r))
    | NSet  Bool [(r, r)]

    | NLet r r
    | NIf r r r
    | NWith r r
    | NAssert r r

    | NVar r
    | NApp r r
    | NAbs r r
      -- ^ The untyped lambda calculus core
    deriving (Ord, Eq, Generic, Typeable, Data)

instance Functor NExprF where
    fmap _ (NConstant a)  = NConstant a
    fmap f (NList r)      = NList (fmap f r)
    fmap f (NConcat r)    = NConcat (fmap f r)
    fmap f (NArgSet h)    = NArgSet (fmap (fmap f) h)
    fmap f (NSet b h)     = NSet b $ map go h
      where go (k, v) = (f k, f v)
    fmap f (NLet r r1)    = NLet (f r) (f r1)
    fmap f (NIf r r1 r2)  = NIf (f r) (f r1) (f r2)
    fmap f (NWith r r1)   = NWith (f r) (f r1)
    fmap f (NAssert r r1) = NAssert (f r) (f r1)
    fmap f (NVar r)       = NVar (f r)
    fmap f (NApp r r1)    = NApp (f r) (f r1)
    fmap f (NAbs r r1)    = NAbs (f r) (f r1)

newtype Fix (f :: * -> *) = Fix { outF :: f (Fix f) }

cata :: Functor f => (f a -> a) -> Fix f -> a
cata f = f . fmap (cata f) . outF

type NExpr = Fix NExprF

newtype NEnv a = NEnv { getEnv :: Map a a }

instance Show f => Show (NExprF f) where
    show (NConstant x) = show x

    show (NList l) = "[ " ++ go l ++ " ]"
      where
        go [] = ""
        go [x] = show x
        go (x:xs) = show x ++ ", " ++ go xs

    show (NConcat l) = go l
      where
        go [] = ""
        go [x] = show x
        go (x:xs) = show x ++ " ++ " ++ go xs

    show (NArgSet h) = "{ " ++ go (Map.toList h) ++ " }"
      where
        go [] = ""
        go [x] = showArg x
        go (x:xs) = showArg x ++ ", " ++ go xs

        showArg (k, Nothing) = unpack k
        showArg (k, Just v) = unpack k ++ " ? " ++ show v

    show (NSet b xs) = (if b then "rec " else "")
                           ++ "{ " ++ concatMap go xs ++ " }"
      where
        go (k, v) = show k ++ " = " ++ show v ++ "; "

    show (NLet v e)    = "let " ++ show v ++ "; " ++ show e
    show (NIf i t e)   = "if " ++ show i ++ " then " ++ show t ++ " else " ++ show e
    show (NWith c v)   = "with " ++ show c ++ "; " ++ show v
    show (NAssert e v) = "assert " ++ show e ++ "; " ++ show v

    show (NVar v)      = show v
    show (NApp f x)    = show f ++ " " ++ show x
    show (NAbs a b)    = show a ++ ": " ++ show b

dumpExpr :: NExpr -> String
dumpExpr = cata phi where
  phi (NConstant x) = "NConstant " ++ dumpAtom x
  phi (NList l)     = "NList [" ++ show l ++ "]"
  phi (NConcat l)   = "NConcat " ++ show l
  phi (NArgSet xs)  = "NArgSet " ++ show xs
  phi (NSet b xs)   = "NSet " ++ show b ++ " " ++ show xs
  phi (NLet v e)    = "NLet " ++ v ++ " " ++ e
  phi (NIf i t e)   = "NIf " ++ i ++ " " ++ t ++ " " ++ e
  phi (NWith c v)   = "NWith " ++ c ++ " " ++ v
  phi (NAssert e v) = "NAssert " ++ e ++ " " ++ v
  phi (NVar v)      = "NVar " ++ v
  phi (NApp f x)    = "NApp " ++ f ++ " " ++ x
  phi (NAbs a b)    = "NAbs " ++ a ++ " " ++ b

mkInt :: Integer -> NExpr
mkInt = Fix . NConstant . NInt

mkStr :: Text -> NExpr
mkStr = Fix . NConstant . NStr

mkPath :: FilePath -> NExpr
mkPath = Fix . NConstant . NPath

mkSym :: Text -> NExpr
mkSym = Fix . NConstant . NSym

mkBool :: Bool -> NExpr
mkBool = Fix . NConstant . NBool

mkNull :: NExpr
mkNull = Fix (NConstant NNull)

instance Show (Fix NExprF) where
    show (Fix f) = show f

instance Eq (Fix NExprF) where
    Fix x == Fix y = x == y

instance Ord (Fix NExprF) where
    compare (Fix x) (Fix y) = compare x y

-- | Given an argument passed to a function, "fit" it within the arguments as
--   specified by the definition of the function.
--
-- For example, a function may be defined as @{ foo, bar }: body@, in which
-- case the argument must be a set that provides at least the keys @foo@ and
-- @bar@.
fitArgument :: NExpr -> NExpr -> NExpr
fitArgument _ _ = undefined

nixApp :: Parser NExpr
nixApp = go <$> some (whiteSpace *> nixTerm True)
  where
    go [x]    = x
    go (f:xs) = Fix (NApp f (go xs))

nixTerm :: Bool -> Parser NExpr
nixTerm allowLambdas = choice
    [ mkInt <$> decimal                       <?> "integer"

    , (string "true" *> pure (mkBool True))   <?> "bool"
    , (string "false" *> pure (mkBool False)) <?> "bool"
    , (string "null" *> pure mkNull)          <?> "null"

    , between (symbolic '(') (symbolic ')') nixApp
          <?> "parens"

    , between (symbolic '[') (symbolic ']')
          (Fix . NList <$> many (nixTerm False))
          <?> "list"

    , try (do chars <- some (satisfy isPathChar)
              trace ("Path chars: " ++ show chars) $ return ()
              guard ('/' `elem` chars)
              return $ mkPath chars)

    , maybeSetOrLambda allowLambdas
    ]

maybeSetOrLambda :: Bool -> Parser NExpr
maybeSetOrLambda allowLambdas = do
    trace "maybeSetOrLambda" $ return ()
    x <- try (lookAhead symName)
        <|> try (lookAhead (singleton <$> char '{'))
        <|> return ""
    if x == "rec" || x == "{"
        then setOrArgs
        else do
            trace "might still have a lambda" $ return ()
            y <- try (lookAhead (symName *> whiteSpace *> symbolic ':'
                                     *> return True))
                <|> return False
            trace ("results are = " ++ show y) $ return ()
            if y
                then if allowLambdas
                    then trace "looking for set" $ setOrArgs
                    else error "Unexpected lambda"
                else trace "just want a string" $ (keyName <?> "string")

isPathChar :: Char -> Bool
isPathChar c = isAlpha c || c `Prelude.elem` ".:/"

oneChar :: Parser NExpr
oneChar = mkStr . singleton <$> anyChar

stringChar :: Parser NExpr
stringChar = char '\\' *> oneChar
         <|> (string "${" *> nixApp <* char '}')
         <|> (mkStr . pack <$> many (noneOf "\"\\"))

symName :: Parser Text
symName = do
    trace "symName" $ return ()
    chars <- some (satisfy (\c -> isAlpha c || c == '.'))
    trace ("chars = " ++ show chars) $ return ()
    guard (isLower (head chars))
    trace ("chars2 = " ++ show chars) $ return ()
    return $ pack (trace ("chars: " ++ show chars) chars)

stringish :: Parser NExpr
stringish
     =  (char '"' *>
         (Fix . NConcat <$> manyTill stringChar (char '"')))
    <|> (char '$' *> between (symbolic '{') (symbolic '}') nixApp)

keyName :: Parser NExpr
keyName = (stringish <|> (mkSym <$> symName)) <* whiteSpace

nvPair :: Parser (NExpr, NExpr)
nvPair = (,) <$> keyName
             <*> (symbolic '=' *> nixApp)

argExpr :: Parser NExpr
argExpr =  (Fix . NArgSet . Map.fromList <$> argList)
       <|> ((mkSym <$> symName) <?> "argname")

argList :: Parser [(Text, Maybe NExpr)]
argList = between (symbolic '{') (symbolic '}')
              ((argName <* whiteSpace) `sepBy` symbolic ',')
              <?> "arglist"

argName :: Parser (Text, Maybe NExpr)
argName = (,) <$> (symName <* whiteSpace)
              <*> optional (try (symbolic '?' *> nixApp))

whiteSymbolic :: Char -> Parser Char
whiteSymbolic c = whiteSpace *> symbolic c

lookaheadForSet :: Parser Bool
lookaheadForSet = do
    trace "lookaheadForSet" $ return ()
    x <- (symbolic '{' *> return True) <|> return False
    if not x then return x else do
        y <- (keyName *> return True) <|> return False
        if not y then return y else
            (symbolic '=' *> return True) <|> return False

setOrArgs :: Parser NExpr
setOrArgs = do
    trace "setOrArgs" $ return ()
    sawRec <- try (symbol "rec" *> pure True) <|> pure False
    trace ("Do we have sawRec: " ++ show sawRec) $ return ()
    haveSet <-
        if sawRec
        then return True
        else try (lookAhead lookaheadForSet)
    trace ("Do we have a set: " ++ show haveSet) $ return ()
    if haveSet
        then between (symbolic '{') (symbolic '}')
                 (Fix . NSet sawRec <$> nvPair `endBy` symbolic ';')
                 <?> "set"
        else do
            trace "parsing arguments" $ return ()
            args <- argExpr <?> "arguments"
            trace ("args: " ++ show args) $ return ()
            symbolic ':' *> ((Fix .) . NAbs <$> pure args <*> nixApp)
                <|> pure args

-- symbol :: String -> Parser String
-- symbol str = string str <* whiteSpace

-- symbolic :: Char -> Parser Char
-- symbolic c = char c <* whiteSpace

-- decimal :: Parser Integer
-- decimal = read <$> some digit

-- whiteSpace :: Parser ()
-- whiteSpace = spaces

-- parseFromFile :: Parser a -> FilePath -> IO (Maybe a)
-- parseFromFile p path = do
--     txt <- readFile path
--     case parse p path txt of
--         Left e  -> error (show e)
--         Right r -> return $ Just r

nix :: FilePath -> IO ()
nix path = do
    mpkgs <- parseFromFile nixApp path
    forM_ mpkgs $ \pkgs -> do
        mem <- once (return pkgs)
        res <- join $ loeb (return (const mem))
        Prelude.print res

main :: IO ()
main = do
    [path] <- getArgs
    nix path
