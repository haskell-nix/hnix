{-# language GeneralizedNewtypeDeriving #-}

module Nix.String
  ( NixString
  , getStringContext
  , mkNixString
  , StringContext(..)
  , ContextFlavor(..)
  , NixLikeContext(..)
  , NixLikeContextValue(..)
  , toNixLikeContext
  , fromNixLikeContext
  , hasContext
  , intercalateNixString
  , getStringNoContext
  , ignoreContext
  , mkNixStringWithoutContext
  , mkNixStringWithSingletonContext
  , modifyNixContents
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




import           Nix.Prelude             hiding ( Type, TVar )
import           Control.Monad.Writer           ( WriterT(..), MonadWriter(tell))
import qualified Data.HashMap.Lazy             as M
import qualified Data.HashSet                  as S
import qualified Data.Text                     as Text
import           Nix.Expr.Types                 ( VarName(..)
                                                , AttrSet
                                                )


-- * Types

-- ** Context

-- | A Nix 'StringContext' ...
data StringContext =
  StringContext
    { getStringContextFlavor :: !ContextFlavor
    , getStringContextPath   :: !VarName
    }
  deriving (Eq, Ord, Show, Generic)

instance Hashable StringContext

-- | A 'ContextFlavor' describes the sum of possible derivations for string contexts
data ContextFlavor
  = DirectPath
  | AllOutputs
  | DerivationOutput !Text
  deriving (Show, Eq, Ord, Generic)

instance Hashable ContextFlavor

newtype NixLikeContext =
  NixLikeContext
    { getNixLikeContext :: AttrSet NixLikeContextValue
    }
  deriving (Eq, Ord, Show, Generic)

data NixLikeContextValue =
  NixLikeContextValue
    { nlcvPath :: !Bool
    , nlcvAllOutputs :: !Bool
    , nlcvOutputs :: ![Text]
    }
  deriving (Show, Eq, Ord, Generic)

instance Semigroup NixLikeContextValue where
  a <> b =
    NixLikeContextValue
      { nlcvPath       = nlcvPath       a || nlcvPath       b
      , nlcvAllOutputs = nlcvAllOutputs a || nlcvAllOutputs b
      , nlcvOutputs    = nlcvOutputs    a <> nlcvOutputs    b
      }

instance Monoid NixLikeContextValue where
  mempty = NixLikeContextValue False False mempty


-- ** StringContext accumulator

-- | A monad for accumulating string context while producing a result string.
newtype WithStringContextT m a =
  WithStringContextT
    (WriterT (S.HashSet StringContext) m a )
  deriving (Functor, Applicative, Monad, MonadTrans, MonadWriter (S.HashSet StringContext))

type WithStringContext = WithStringContextT Identity


-- ** NixString

data NixString =
  NixString
    { getStringContext :: !(S.HashSet StringContext)
    , getStringContent :: !Text
    }
  deriving (Eq, Ord, Show, Generic)

instance Semigroup NixString where
  NixString s1 t1 <> NixString s2 t2 = NixString (s1 <> s2) (t1 <> t2)

instance Monoid NixString where
 mempty = NixString mempty mempty

instance Hashable NixString


-- * Functions

-- ** Makers

-- | Constructs NixString without a context
mkNixStringWithoutContext :: Text -> NixString
mkNixStringWithoutContext = NixString mempty

-- | Create NixString using a singleton context
mkNixStringWithSingletonContext
  :: StringContext -> VarName -> NixString
mkNixStringWithSingletonContext c s = NixString (one c) (coerce @VarName @Text s)

-- | Create NixString from a Text and context
mkNixString
  :: S.HashSet StringContext -> Text -> NixString
mkNixString = NixString


-- ** Checkers

-- | Returns True if the NixString has an associated context
hasContext :: NixString -> Bool
hasContext (NixString c _) = isPresent c


-- ** Getters

fromNixLikeContext :: NixLikeContext -> S.HashSet StringContext
fromNixLikeContext =
  S.fromList . (uncurry toStringContexts <=< M.toList . getNixLikeContext)

-- | Extract the string contents from a NixString that has no context
getStringNoContext :: NixString -> Maybe Text
getStringNoContext (NixString c s)
  | null c    = pure s
  | otherwise = mempty

-- | Extract the string contents from a NixString even if the NixString has an associated context
ignoreContext :: NixString -> Text
ignoreContext (NixString _ s) = s

-- | Get the contents of a 'NixString' and write its context into the resulting set.
extractNixString :: Monad m => NixString -> WithStringContextT m Text
extractNixString (NixString c s) =
  WithStringContextT $
    s <$ tell c


-- ** Setters

-- this really should be 2 args, then with @toStringContexts path@ laziness it would tail recurse.
-- for now tuple dissected internaly with laziness preservation.
toStringContexts :: VarName -> NixLikeContextValue -> [StringContext]
toStringContexts path = go
 where
  go :: NixLikeContextValue -> [StringContext]
  go cv =
    case cv of
      NixLikeContextValue True _    _ ->
        mkLstCtxFor DirectPath cv { nlcvPath = False }
      NixLikeContextValue _    True _ ->
        mkLstCtxFor AllOutputs cv { nlcvAllOutputs = False }
      NixLikeContextValue _    _    ls | isPresent ls ->
        mkCtxFor . DerivationOutput <$> ls
      _ -> mempty
   where
    mkCtxFor :: ContextFlavor -> StringContext
    mkCtxFor context = StringContext context path
    mkLstCtxFor :: ContextFlavor -> NixLikeContextValue -> [StringContext]
    mkLstCtxFor t c = one (mkCtxFor t) <> go c


toNixLikeContextValue :: StringContext -> (NixLikeContextValue, VarName)
toNixLikeContextValue sc =
  ( case getStringContextFlavor sc of
      DirectPath         -> NixLikeContextValue True False mempty
      AllOutputs         -> NixLikeContextValue False True mempty
      DerivationOutput t -> NixLikeContextValue False False $ one t
  , getStringContextPath sc
  )

toNixLikeContext :: S.HashSet StringContext -> NixLikeContext
toNixLikeContext stringContext =
  NixLikeContext $
    S.foldr
      fun
      mempty
      stringContext
 where
  fun :: (StringContext -> AttrSet NixLikeContextValue -> AttrSet NixLikeContextValue)
  fun sc =
    uncurry (M.insertWith (<>)) (swap $ toNixLikeContextValue sc)

-- | Add 'StringContext's into the resulting set.
addStringContext
  :: Monad m => S.HashSet StringContext -> WithStringContextT m ()
addStringContext = WithStringContextT . tell

-- | Add a 'StringContext' into the resulting set.
addSingletonStringContext :: Monad m => StringContext -> WithStringContextT m ()
addSingletonStringContext = WithStringContextT . tell . one

-- | Run an action producing a string with a context and put those into a 'NixString'.
runWithStringContextT :: Monad m => WithStringContextT m Text -> m NixString
runWithStringContextT (WithStringContextT m) =
  uncurry (flip NixString) <$> runWriterT m

-- | Run an action producing a string with a context and put those into a 'NixString'.
runWithStringContext :: WithStringContextT Identity Text -> NixString
runWithStringContext = runIdentity . runWithStringContextT


-- ** Modifiers

-- | Modify the string part of the NixString, leaving the context unchanged
modifyNixContents :: (Text -> Text) -> NixString -> NixString
modifyNixContents f (NixString c s) = NixString c (f s)

-- | Run an action that manipulates nix strings, and collect the contexts encountered.
-- Warning: this may be unsafe, depending on how you handle the resulting context list.
runWithStringContextT' :: Monad m => WithStringContextT m a -> m (a, S.HashSet StringContext)
runWithStringContextT' (WithStringContextT m) = runWriterT m

-- | Run an action that manipulates nix strings, and collect the contexts encountered.
-- Warning: this may be unsafe, depending on how you handle the resulting context list.
runWithStringContext' :: WithStringContextT Identity a -> (a, S.HashSet StringContext)
runWithStringContext' = runIdentity . runWithStringContextT'

-- | Combine NixStrings with a separator
intercalateNixString :: NixString -> [NixString] -> NixString
intercalateNixString _   []   = mempty
intercalateNixString _   [ns] = ns
intercalateNixString sep nss  =
  uncurry NixString $
    mapPair
      (S.unions . (one (getStringContext  sep) <>) . (getStringContext <$>)
      , Text.intercalate (getStringContent sep) . (getStringContent <$>)
      )
      $ dup nss
