{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Nix.String
  (
    principledGetContext
  , principledMakeNString
  , principledMempty
  , StringContext(..)
  , ContextFlavor(..)
  , NixLikeContext(..)
  , NixLikeContextValue(..)
  , toNixLikeContext
  , fromNixLikeContext
  , stringHasContext
  , principledIntercalateNString
  , hackyGetStringNoContext
  , principledGetStringNoContext
  , principledStringIgnoreContext
  , hackyStringIgnoreContext
  , hackyMakeNStringWithoutContext
  , principledMakeNStringWithoutContext
  , principledMakeNStringWithSingletonContext
  , principledModifyNixContents
  , principledStringMappend
  , principledStringMempty
  , principledStringMConcat
  , WithStringContext
  , WithStringContextT(..)
  , extractNString
  , addStringContext
  , addSingletonStringContext
  , runWithStringContextT
  , runWithStringContext
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
import           Nix.Atoms
-- ( NAtom (NString)
--                                            , StringContext (StringContext (scPath))
--                                            , ContextFlavor (DirectPath)
--                                            , ContextFlavor (AllOutputs)
--                                            , ContextFlavor (DerivationOutput)
--                                            )

-- {-# WARNING hackyGetStringNoContext, hackyStringIgnoreContext, hackyMakeNStringWithoutContext "This NString function needs to be replaced" #-}

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

principledGetContext :: NAtom -> S.HashSet StringContext
principledGetContext = nsContext

-- | Combine two NStrings using mappend
principledMempty :: NAtom
principledMempty = NString "" mempty

-- | Combine two NStrings using mappend
principledStringMappend :: NAtom -> NAtom -> NAtom
principledStringMappend (NString s1 t1) (NString s2 t2) =
  NString (s1 <> s2) (t1 <> t2)

-- | Combine two NStrings using mappend
hackyStringMappend :: NAtom -> NAtom -> NAtom
hackyStringMappend (NString s1 t1) (NString s2 t2) =
  NString (s1 <> s2) (t1 <> t2)

-- | Combine (NString :: NAtom) with a separator
principledIntercalateNString :: NAtom -> [NAtom] -> NAtom
principledIntercalateNString _   []   = principledMempty
principledIntercalateNString _   [ns] = ns
principledIntercalateNString sep nss  = NString contents ctx
 where
  contents = Text.intercalate (nsContents sep) (map nsContents nss)
  ctx      = S.unions (nsContext sep : map nsContext nss)

-- | Combine NStrings using mconcat
hackyStringMConcat :: [NAtom] -> NAtom
hackyStringMConcat = foldr hackyStringMappend (NString mempty mempty)

-- | Empty string with empty context.
principledStringMempty :: NAtom
principledStringMempty = NString mempty mempty

-- | Combine NStrings using mconcat
principledStringMConcat :: [NAtom] -> NAtom
principledStringMConcat =
  foldr principledStringMappend (NString mempty mempty)

--instance Semigroup NString where
  --NString s1 t1 <> NString s2 t2 = NString (s1 <> s2) (t1 <> t2)

--instance Monoid NString where
--  mempty = NString mempty mempty
--  mappend = (<>)

-- | Extract the string contents from a NString that has no context
hackyGetStringNoContext :: NAtom -> Maybe Text
hackyGetStringNoContext (NString s c) | null c    = Just s
                                        | otherwise = Nothing

-- | Extract the string contents from a NString that has no context
principledGetStringNoContext :: NAtom -> Maybe Text
principledGetStringNoContext (NString s c) | null c    = Just s
                                             | otherwise = Nothing

-- | Extract the string contents from a NString even if the NString has an associated context
principledStringIgnoreContext :: NAtom -> Text
principledStringIgnoreContext (NString s _) = s

-- | Extract the string contents from a NString even if the NString has an associated context
hackyStringIgnoreContext :: NAtom -> Text
hackyStringIgnoreContext (NString s _) = s

-- | Returns True if the NString has an associated context
stringHasContext :: NAtom -> Bool
stringHasContext (NString _ c) = not (null c)

-- | Constructs a NString without a context
hackyMakeNStringWithoutContext :: Text -> NAtom
hackyMakeNStringWithoutContext = flip NString mempty

-- | Constructs a NString without a context
principledMakeNStringWithoutContext :: Text -> NAtom
principledMakeNStringWithoutContext = flip NString mempty

-- | Modify the string part of the NString, leaving the context unchanged
principledModifyNixContents :: (Text -> Text) -> NAtom -> NAtom
principledModifyNixContents f (NString s c) = NString (f s) c

-- | Create a NString using a singleton context
principledMakeNStringWithSingletonContext
  :: Text -> StringContext -> NAtom
principledMakeNStringWithSingletonContext s c = NString s (S.singleton c)

-- | Create a NString from a Text and context
principledMakeNString :: Text -> S.HashSet StringContext -> NAtom
principledMakeNString s c = NString s c

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

-- | Get the contents of a 'NString :: NAtom' and write its context into the resulting set.
extractNString :: Monad m => NAtom -> WithStringContextT m Text
extractNString (NString s c) = WithStringContextT $ tell c >> return s

-- | Run an action producing a string with a context and put those into a 'NString'.
runWithStringContextT :: Monad m => WithStringContextT m Text -> m NAtom
runWithStringContextT (WithStringContextT m) =
  uncurry NString <$> runWriterT m

-- | Run an action producing a string with a context and put those into a 'NString'.
runWithStringContext :: WithStringContextT Identity Text -> NAtom
runWithStringContext = runIdentity . runWithStringContextT
