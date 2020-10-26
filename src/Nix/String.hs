{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Nix.String
  ( NixString
  , principledGetContext
  , principledMakeNixString
  , principledMempty
  , StringContext(..)
  , ContextFlavor(..)
  , NixLikeContext(..)
  , NixLikeContextValue(..)
  , toNixLikeContext
  , fromNixLikeContext
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
  , WithStringContext
  , WithStringContextT(..)
  , extractNixString
  , addStringContext
  , addSingletonStringContext
  , runWithStringContextT
  , runWithStringContextT'
  , runWithStringContext
  , runWithStringContext'
  )
where

import           Control.Monad.Writer
import           Data.Functor.Identity
import qualified Data.HashMap.Lazy             as M
import qualified Data.HashSet                  as S
import           Data.Hashable
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           GHC.Generics

-- {-# WARNING hackyGetStringNoContext, hackyStringIgnoreContext, hackyMakeNixStringWithoutContext "This NixString function needs to be replaced" #-}

-- | A 'ContextFlavor' describes the sum of possible derivations for string contexts
data ContextFlavor =
    DirectPath
  | AllOutputs
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

newtype NixLikeContext = NixLikeContext
  { getNixLikeContext :: M.HashMap Text NixLikeContextValue
  } deriving (Eq, Ord, Show, Generic)

data NixLikeContextValue = NixLikeContextValue
  { nlcvPath :: !Bool
  , nlcvAllOutputs :: !Bool
  , nlcvOutputs :: ![Text]
  } deriving (Show, Eq, Ord, Generic)

instance Semigroup NixLikeContextValue where
  a <> b = NixLikeContextValue
    { nlcvPath       = nlcvPath a || nlcvPath b
    , nlcvAllOutputs = nlcvAllOutputs a || nlcvAllOutputs b
    , nlcvOutputs    = nlcvOutputs a <> nlcvOutputs b
    }

instance Monoid NixLikeContextValue where
  mempty = NixLikeContextValue False False []

toStringContexts :: (Text, NixLikeContextValue) -> [StringContext]
toStringContexts (path, nlcv) = case nlcv of
  NixLikeContextValue True _ _ -> StringContext path DirectPath
    : toStringContexts (path, nlcv { nlcvPath = False })
  NixLikeContextValue _ True _ -> StringContext path AllOutputs
    : toStringContexts (path, nlcv { nlcvAllOutputs = False })
  NixLikeContextValue _ _ ls | not (null ls) ->
    map (StringContext path . DerivationOutput) ls
  _ -> []

toNixLikeContextValue :: StringContext -> (Text, NixLikeContextValue)
toNixLikeContextValue sc = (,) (scPath sc) $ case scFlavor sc of
  DirectPath         -> NixLikeContextValue True False []
  AllOutputs         -> NixLikeContextValue False True []
  DerivationOutput t -> NixLikeContextValue False False [t]

toNixLikeContext :: S.HashSet StringContext -> NixLikeContext
toNixLikeContext stringContext = NixLikeContext
  $ S.foldr go mempty stringContext
 where
  go sc hm =
    let (t, nlcv) = toNixLikeContextValue sc in M.insertWith (<>) t nlcv hm

fromNixLikeContext :: NixLikeContext -> S.HashSet StringContext
fromNixLikeContext =
  S.fromList . join . map toStringContexts . M.toList . getNixLikeContext

principledGetContext :: NixString -> S.HashSet StringContext
principledGetContext = nsContext

-- | Combine two NixStrings using mappend
principledMempty :: NixString
principledMempty = NixString "" mempty

-- | Combine two NixStrings using mappend
principledStringMappend :: NixString -> NixString -> NixString
principledStringMappend (NixString s1 t1) (NixString s2 t2) =
  NixString (s1 <> s2) (t1 <> t2)

-- | Combine two NixStrings using mappend
hackyStringMappend :: NixString -> NixString -> NixString
hackyStringMappend (NixString s1 t1) (NixString s2 t2) =
  NixString (s1 <> s2) (t1 <> t2)

-- | Combine NixStrings with a separator
principledIntercalateNixString :: NixString -> [NixString] -> NixString
principledIntercalateNixString _   []   = principledMempty
principledIntercalateNixString _   [ns] = ns
principledIntercalateNixString sep nss  = NixString contents ctx
 where
  contents = Text.intercalate (nsContents sep) (map nsContents nss)
  ctx      = S.unions (nsContext sep : map nsContext nss)

-- | Combine NixStrings using mconcat
hackyStringMConcat :: [NixString] -> NixString
hackyStringMConcat = foldr hackyStringMappend (NixString mempty mempty)

-- | Empty string with empty context.
principledStringMempty :: NixString
principledStringMempty = NixString mempty mempty

-- | Combine NixStrings using mconcat
principledStringMConcat :: [NixString] -> NixString
principledStringMConcat =
  foldr principledStringMappend (NixString mempty mempty)

--instance Semigroup NixString where
  --NixString s1 t1 <> NixString s2 t2 = NixString (s1 <> s2) (t1 <> t2)

--instance Monoid NixString where
--  mempty = NixString mempty mempty
--  mappend = (<>)

-- | Extract the string contents from a NixString that has no context
hackyGetStringNoContext :: NixString -> Maybe Text
hackyGetStringNoContext (NixString s c) | null c    = Just s
                                        | otherwise = Nothing

-- | Extract the string contents from a NixString that has no context
principledGetStringNoContext :: NixString -> Maybe Text
principledGetStringNoContext (NixString s c) | null c    = Just s
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
principledMakeNixStringWithSingletonContext
  :: Text -> StringContext -> NixString
principledMakeNixStringWithSingletonContext s c = NixString s (S.singleton c)

-- | Create a NixString from a Text and context
principledMakeNixString :: Text -> S.HashSet StringContext -> NixString
principledMakeNixString = NixString

-- | A monad for accumulating string context while producing a result string.
newtype WithStringContextT m a = WithStringContextT (WriterT (S.HashSet StringContext) m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadWriter (S.HashSet StringContext))

type WithStringContext = WithStringContextT Identity

-- | Add 'StringContext's into the resulting set.
addStringContext
  :: Monad m => S.HashSet StringContext -> WithStringContextT m ()
addStringContext = WithStringContextT . tell

-- | Add a 'StringContext' into the resulting set.
addSingletonStringContext :: Monad m => StringContext -> WithStringContextT m ()
addSingletonStringContext = WithStringContextT . tell . S.singleton

-- | Get the contents of a 'NixString' and write its context into the resulting set.
extractNixString :: Monad m => NixString -> WithStringContextT m Text
extractNixString (NixString s c) = WithStringContextT $ tell c >> pure s

-- | Run an action producing a string with a context and put those into a 'NixString'.
runWithStringContextT :: Monad m => WithStringContextT m Text -> m NixString
runWithStringContextT (WithStringContextT m) =
  uncurry NixString <$> runWriterT m

-- | Run an action that manipulates nix strings, and collect the contexts encountered.
-- Warning: this may be unsafe, depending on how you handle the resulting context list.
runWithStringContextT' :: Monad m => WithStringContextT m a -> m (a, S.HashSet StringContext)
runWithStringContextT' (WithStringContextT m) = runWriterT m

-- | Run an action producing a string with a context and put those into a 'NixString'.
runWithStringContext :: WithStringContextT Identity Text -> NixString
runWithStringContext = runIdentity . runWithStringContextT

-- | Run an action that manipulates nix strings, and collect the contexts encountered.
-- Warning: this may be unsafe, depending on how you handle the resulting context list.
runWithStringContext' :: WithStringContextT Identity a -> (a, S.HashSet StringContext)
runWithStringContext' = runIdentity . runWithStringContextT'
