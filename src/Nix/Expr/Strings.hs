
-- | Functions for manipulating nix strings.
module Nix.Expr.Strings where

import           Relude.Unsafe                 as Unsafe
-- Please, switch things to NonEmpty
import           Data.List                      ( dropWhileEnd
                                                , minimum
                                                , lookup
                                                )
import qualified Data.Text                     as T
import           Nix.Expr.Types

-- | Merge adjacent @Plain@ values with @<>@.
mergePlain :: [Antiquoted Text r] -> [Antiquoted Text r]
mergePlain [] = mempty
mergePlain (Plain a : EscapedNewline : Plain b : xs) =
  mergePlain (Plain (a <> "\n" <> b) : xs)
mergePlain (Plain a : Plain b : xs) = mergePlain (Plain (a <> b) : xs)
mergePlain (x                 : xs) = x : mergePlain xs

-- | Remove 'Plain' values equal to 'mempty', as they don't have any
-- informational content.
removeEmptyPlains :: [Antiquoted Text r] -> [Antiquoted Text r]
removeEmptyPlains = filter f where
  f (Plain x) = x /= mempty
  f _         = True

  -- trimEnd xs
  --     | null xs = xs
  --     | otherwise = case last xs of
  --           Plain x -> init xs <> [Plain (T.dropWhileEnd (== ' ') x)]
  --           _ -> xs

-- | Equivalent to case splitting on 'Antiquoted' strings.
runAntiquoted :: v -> (v -> a) -> (r -> a) -> Antiquoted v r -> a
runAntiquoted _  f _ (Plain v)      = f v
runAntiquoted nl f _ EscapedNewline = f nl
runAntiquoted _  _ k (Antiquoted r) = k r

-- | Split a stream representing a string with antiquotes on line breaks.
splitLines :: forall r . [Antiquoted Text r] -> [[Antiquoted Text r]]
splitLines = uncurry (flip (:)) . go
 where
  go :: [Antiquoted Text r] -> ([[Antiquoted Text r]], [Antiquoted Text r])
  go (Plain t : xs) = (one (Plain l) <>) <$> foldr f (go xs) ls
   where
    (l : ls) = T.split (== '\n') t
    f prefix (finished, current) = ((Plain prefix : current) : finished, mempty)
  go (Antiquoted a   : xs) = (one (Antiquoted a) <>) <$> go xs
  go (EscapedNewline : xs) = (one EscapedNewline <>) <$> go xs
  go []                    = mempty

-- | Join a stream of strings containing antiquotes again. This is the inverse
-- of 'splitLines'.
unsplitLines :: [[Antiquoted Text r]] -> [Antiquoted Text r]
unsplitLines = intercalate $ one $ Plain "\n"

-- | Form an indented string by stripping spaces equal to the minimal indent.
stripIndent :: [Antiquoted Text r] -> NString r
stripIndent [] = Indented 0 mempty
stripIndent xs =
  Indented
    minIndent
    (removeEmptyPlains $
      mergePlain $
        (snd <$>) $
          dropWhileEnd
            cleanup
            $ pairWithLast $ unsplitLines ls'
    )
 where
  pairWithLast ys =
    zip
      (list
        Nothing
        (pure . Unsafe.last)
        <$> inits ys
      )
      ys

  ls        = stripEmptyOpening $ splitLines xs
  ls'       = dropSpaces minIndent <$> ls

  minIndent =
    list
      0
      (minimum . (countSpaces . mergePlain <$>))
      (stripEmptyLines ls)

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
  dropSpaces _ _              = fail "stripIndent: impossible"

  cleanup (Nothing, Plain y) = T.all (== ' ') y
  cleanup (Just (Plain x), Plain y) | "\n" `T.isSuffixOf` x = T.all (== ' ') y
  cleanup _                  = False

escapeCodes :: [(Char, Char)]
escapeCodes =
  [('\n', 'n'), ('\r', 'r'), ('\t', 't'), ('\\', '\\'), ('$', '$'), ('"', '"')]

fromEscapeCode :: Char -> Maybe Char
fromEscapeCode = (`lookup` (swap <$> escapeCodes))

toEscapeCode :: Char -> Maybe Char
toEscapeCode = (`lookup` escapeCodes)
