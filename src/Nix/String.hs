{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Nix.String (
    NixString
  , principledGetContext
  , principledMakeNixString
  , principledMempty
  , StringContext(..)
  , ContextFlavor(..)
  , stringHasContext
  , principledIntercalateNixString
  , hackyGetStringNoContext
  , principledGetStringNoContext
  , principledStringIgnoreContext
  , hackyStringIgnoreContext
  , hackyMakeNixStringWithoutContext
  , principledMakeNixStringWithoutContext
  , principledMakeNixStringWithSingletonContext
  , principledModifyNixContents
  , principledStringMappend
  , principledStringMempty
  , principledStringMConcat
) where

import qualified Data.HashSet as S
import           Data.Hashable
import           Data.Text (Text)
import qualified Data.Text as Text
import           GHC.Generics

-- {-# WARNING hackyGetStringNoContext, hackyStringIgnoreContext, hackyMakeNixStringWithoutContext "This NixString function needs to be replaced" #-}

-- | A 'ContextFlavor' describes the sum of possible derivations for string contexts
data ContextFlavor =
    DirectPath
  | DerivationOutput !Text
  deriving (Show, Eq, Ord, Generic)

instance Hashable ContextFlavor

-- | A 'StringContext' ...
data StringContext =
  StringContext { scPath :: !Text
                , scFlavor :: !ContextFlavor
                } deriving (Eq, Ord, Show, Generic)

instance Hashable StringContext

data NixString = NixString
  { nsContents :: !Text
  , nsContext :: !(S.HashSet StringContext)
  } deriving (Eq, Ord, Show, Generic)

instance Hashable NixString

principledGetContext :: NixString -> S.HashSet StringContext
principledGetContext = nsContext

-- | Combine two NixStrings using mappend
principledMempty :: NixString
principledMempty = NixString "" mempty

-- | Combine two NixStrings using mappend
principledStringMappend :: NixString -> NixString -> NixString
principledStringMappend (NixString s1 t1) (NixString s2 t2) = NixString (s1 <> s2) (t1 <> t2)

-- | Combine two NixStrings using mappend
hackyStringMappend :: NixString -> NixString -> NixString
hackyStringMappend (NixString s1 t1) (NixString s2 t2) = NixString (s1 <> s2) (t1 <> t2)

-- | Combine NixStrings with a separator
principledIntercalateNixString :: NixString -> [NixString] -> NixString
principledIntercalateNixString _ [] = principledMempty
principledIntercalateNixString _ [ns] = ns
principledIntercalateNixString sep nss = NixString contents ctx
  where
    contents = Text.intercalate (nsContents sep) (map nsContents nss)
    ctx = S.unions (nsContext sep : map nsContext nss)

-- | Combine NixStrings using mconcat
hackyStringMConcat :: [NixString] -> NixString
hackyStringMConcat = foldr hackyStringMappend (NixString mempty mempty)

-- | Empty string with empty context.
principledStringMempty :: NixString
principledStringMempty = NixString mempty mempty

-- | Combine NixStrings using mconcat
principledStringMConcat :: [NixString] -> NixString
principledStringMConcat = foldr principledStringMappend (NixString mempty mempty)

--instance Semigroup NixString where
  --NixString s1 t1 <> NixString s2 t2 = NixString (s1 <> s2) (t1 <> t2)

--instance Monoid NixString where
--  mempty = NixString mempty mempty
--  mappend = (<>)

-- | Extract the string contents from a NixString that has no context
hackyGetStringNoContext :: NixString -> Maybe Text
hackyGetStringNoContext (NixString s c) | null c = Just s
                                | otherwise = Nothing

-- | Extract the string contents from a NixString that has no context
principledGetStringNoContext :: NixString -> Maybe Text
principledGetStringNoContext (NixString s c) | null c = Just s
                                     | otherwise = Nothing

-- | Extract the string contents from a NixString even if the NixString has an associated context
principledStringIgnoreContext :: NixString -> Text
principledStringIgnoreContext (NixString s _) = s

-- | Extract the string contents from a NixString even if the NixString has an associated context
hackyStringIgnoreContext :: NixString -> Text
hackyStringIgnoreContext (NixString s _) = s

-- | Returns True if the NixString has an associated context
stringHasContext :: NixString -> Bool
stringHasContext (NixString _ c) = not (null c)

-- | Constructs a NixString without a context
hackyMakeNixStringWithoutContext :: Text -> NixString
hackyMakeNixStringWithoutContext = flip NixString mempty

-- | Constructs a NixString without a context
principledMakeNixStringWithoutContext :: Text -> NixString
principledMakeNixStringWithoutContext = flip NixString mempty

-- | Modify the string part of the NixString, leaving the context unchanged
principledModifyNixContents :: (Text -> Text) -> NixString -> NixString
principledModifyNixContents f (NixString s c) = NixString (f s) c

-- | Create a NixString using a singleton context
principledMakeNixStringWithSingletonContext :: Text -> StringContext -> NixString
principledMakeNixStringWithSingletonContext s c = NixString s (S.singleton c)

-- | Create a NixString from a Text and context
principledMakeNixString :: Text -> S.HashSet StringContext -> NixString
principledMakeNixString s c = NixString s c
