{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Functions for manipulating nix strings.
module Nix.Expr.Strings where

import           Data.List                      ( intercalate
                                                , dropWhileEnd
                                                , inits
                                                )
import           Data.Monoid                    ( (<>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Tuple                     ( swap )
import           Nix.Expr

-- | Merge adjacent 'Plain' values with 'mappend'.
mergePlain :: [Antiquoted Text r] -> [Antiquoted Text r]
mergePlain [] = []
mergePlain (Plain a : EscapedNewline : Plain b : xs) =
  mergePlain (Plain (a <> "\n" <> b) : xs)
mergePlain (Plain a : Plain b : xs) = mergePlain (Plain (a <> b) : xs)
mergePlain (x                 : xs) = x : mergePlain xs

-- | Remove 'Plain' values equal to 'mempty', as they don't have any
-- informational content.
removePlainEmpty :: [Antiquoted Text r] -> [Antiquoted Text r]
removePlainEmpty = filter f where
  f (Plain x) = x /= mempty
  f _         = True

  -- trimEnd xs
  --     | null xs = xs
  --     | otherwise = case last xs of
  --           Plain x -> init xs ++ [Plain (T.dropWhileEnd (== ' ') x)]
  --           _ -> xs

-- | Equivalent to case splitting on 'Antiquoted' strings.
runAntiquoted :: v -> (v -> a) -> (r -> a) -> Antiquoted v r -> a
runAntiquoted _  f _ (Plain v)      = f v
runAntiquoted nl f _ EscapedNewline = f nl
runAntiquoted _  _ k (Antiquoted r) = k r

-- | Split a stream representing a string with antiquotes on line breaks.
splitLines :: [Antiquoted Text r] -> [[Antiquoted Text r]]
splitLines = uncurry (flip (:)) . go where
  go (Plain t : xs) = (Plain l :) <$> foldr f (go xs) ls   where
    (l : ls) = T.split (== '\n') t
    f prefix (finished, current) = ((Plain prefix : current) : finished, [])
  go (Antiquoted a   : xs) = (Antiquoted a :) <$> go xs
  go (EscapedNewline : xs) = (EscapedNewline :) <$> go xs
  go []                    = ([], [])

-- | Join a stream of strings containing antiquotes again. This is the inverse
-- of 'splitLines'.
unsplitLines :: [[Antiquoted Text r]] -> [Antiquoted Text r]
unsplitLines = intercalate [Plain "\n"]

-- | Form an indented string by stripping spaces equal to the minimal indent.
stripIndent :: [Antiquoted Text r] -> NString r
stripIndent [] = Indented 0 []
stripIndent xs =
  Indented minIndent
    . removePlainEmpty
    . mergePlain
    . map snd
    . dropWhileEnd cleanup
    . (\ys -> zip
        (map
          (\case
            [] -> Nothing
            x  -> Just (last x)
          )
          (inits ys)
        )
        ys
      )
    . unsplitLines
    $ ls'
 where
  ls        = stripEmptyOpening $ splitLines xs
  ls'       = map (dropSpaces minIndent) ls

  minIndent = case stripEmptyLines ls of
    []         -> 0
    nonEmptyLs -> minimum $ map (countSpaces . mergePlain) nonEmptyLs

  stripEmptyLines = filter $ \case
    [Plain t] -> not $ T.null $ T.strip t
    _         -> True

  stripEmptyOpening ([Plain t] : ts) | T.null (T.strip t) = ts
  stripEmptyOpening ts = ts

  countSpaces (Antiquoted _   : _) = 0
  countSpaces (EscapedNewline : _) = 0
  countSpaces (Plain t        : _) = T.length . T.takeWhile (== ' ') $ t
  countSpaces []                   = 0

  dropSpaces 0 x              = x
  dropSpaces n (Plain t : cs) = Plain (T.drop n t) : cs
  dropSpaces _ _              = error "stripIndent: impossible"

  cleanup (Nothing, Plain y) = T.all (== ' ') y
  cleanup (Just (Plain x), Plain y) | "\n" `T.isSuffixOf` x = T.all (== ' ') y
  cleanup _                  = False

escapeCodes :: [(Char, Char)]
escapeCodes =
  [('\n', 'n'), ('\r', 'r'), ('\t', 't'), ('\\', '\\'), ('$', '$'), ('"', '"')]

fromEscapeCode :: Char -> Maybe Char
fromEscapeCode = (`lookup` map swap escapeCodes)

toEscapeCode :: Char -> Maybe Char
toEscapeCode = (`lookup` escapeCodes)
