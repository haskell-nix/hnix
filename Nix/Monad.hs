{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Nix.Monad where

import           Control.Monad.Fix
import           Data.Fix
import qualified Data.Map.Lazy as Map
import           Data.Monoid (appEndo)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Typeable (Typeable)
import           GHC.Generics
import           Nix.Atoms
import           Nix.Expr.Types
import           Nix.Utils

-- | An 'NValue' is the most reduced form of an 'NExpr' after evaluation
-- is completed.
data NValueF m r
    = NVConstant NAtom
     -- | A string has a value and a context, which can be used to record what a
     -- string has been build from
    | NVStr Text (DList Text)
    | NVList [r]
    | NVSet (Map.Map Text r)
    | NVFunction (Params (m r)) (m r)
      -- ^ A function is a closed set of parameters representing the "call
      --   signature", used at application time to check the type of arguments
      --   passed to the function. Since it supports default values which may
      --   depend on other values within the final argument set, this
      --   dependency is represented as a set of pending evaluations. The
      --   arguments are finally normalized into a set which is passed to the
      --   function.
      --
      --   Note that 'm r' is being used here because effectively a function
      --   and its set of default arguments is "never fully evaluated". This
      --   enforces in the type that it must be re-evaluated for each call.
    | NVLiteralPath FilePath
    | NVEnvPath FilePath
    | NVBuiltin String (NThunk m -> m (NValue m))
      -- ^ A builtin function is itself already in normal form. Also, it may
      --   or may not choose to evaluate its argument in the production of a
      --   result.
    deriving (Generic, Typeable, Functor)

-- | An 'NValueNF' is a fully evaluated value in normal form. An 'NValue m' is
--   a value in head normal form, where only the "top layer" has been
--   evaluated. An action of type 'm (NValue m)' is a pending evualation that
--   has yet to be performed. An 'NThunk m' is either a pending evaluation, or
--   a value in head normal form. A 'ValueSet' is a set of mappings from keys
--   to thunks.

type NValueNF m = Fix (NValueF m)      -- normal form
type NValue m   = NValueF m (NThunk m) -- head normal form
type ValueSet m = Map.Map Text (NThunk m)

instance Show f => Show (NValueF m f) where
    showsPrec = flip go where
      go (NVConstant atom)    = showsCon1 "NVConstant" atom
      go (NVStr text context) = showsCon2 "NVStr"      text (appEndo context [])
      go (NVList     list)    = showsCon1 "NVList"     list
      go (NVSet     attrs)    = showsCon1 "NVSet"      attrs
      go (NVFunction r _)     = showsCon1 "NVFunction" (() <$ r)
      go (NVLiteralPath p)    = showsCon1 "NVLiteralPath" p
      go (NVEnvPath p)        = showsCon1 "NVEnvPath" p
      go (NVBuiltin name _)   = showsCon1 "NVBuiltin" name

      showsCon1 :: Show a => String -> a -> Int -> String -> String
      showsCon1 con a d =
          showParen (d > 10) $ showString (con ++ " ") . showsPrec 11 a

      showsCon2 :: (Show a, Show b)
                => String -> a -> b -> Int -> String -> String
      showsCon2 con a b d =
          showParen (d > 10)
              $ showString (con ++ " ")
              . showsPrec 11 a
              . showString " "
              . showsPrec 11 b

valueText :: forall m. MonadNix m => NValueNF m -> m (Text, DList Text)
valueText = cata phi where
    phi :: NValueF m (m (Text, DList Text)) -> m (Text, DList Text)
    phi (NVConstant a)    = pure (atomText a, mempty)
    phi (NVStr t c)       = pure (t, c)
    phi (NVList _)        = error "Cannot coerce a list to a string"
    phi (NVSet set)
      | Just asString <-
        -- TODO: Should this be run through valueText recursively?
        Map.lookup "__asString" set = asString
      | otherwise = error "Cannot coerce a set to a string"
    phi (NVFunction _ _)  = error "Cannot coerce a function to a string"
    phi (NVLiteralPath originalPath) = do
        -- TODO: Capture and use the path of the file being processed as the
        -- base path
        storePath <- addPath originalPath
        pure (Text.pack $ unStorePath storePath, mempty)
    phi (NVEnvPath p)     =
        -- TODO: Ensure this is a store path
        pure (Text.pack p, mempty)
    phi (NVBuiltin _ _)    = error "Cannot coerce a function to a string"

valueTextNoContext :: MonadNix m => NValueNF m -> m Text
valueTextNoContext = fmap fst . valueText

builtin :: MonadNix m => String -> (NThunk m -> m (NValue m)) -> m (NValue m)
builtin name f = return $ NVBuiltin name f

builtin2 :: MonadNix m
         => String -> (NThunk m -> NThunk m -> m (NValue m)) -> m (NValue m)
builtin2 name f = builtin name (builtin name . f)

builtin3 :: MonadNix m
         => String -> (NThunk m -> NThunk m -> NThunk m -> m (NValue m))
         -> m (NValue m)
builtin3 name f =
    builtin name $ \a -> builtin name $ \b -> builtin name $ \c -> f a b c

-- | A path into the nix store
newtype StorePath = StorePath { unStorePath :: FilePath }

class (Show (NScopes m), MonadFix m) => MonadNix m where
    data NScopes m :: *

    currentScope  :: m (NScopes m)
    clearScopes   :: m r -> m r
    pushScope     :: ValueSet m -> m r -> m r
    pushWeakScope :: ValueSet m -> m r -> m r
    pushScopes    :: NScopes m  -> m r -> m r
    lookupVar     :: Text -> m (Maybe (NValue m))

    data NThunk m :: *

    valueRef   :: NValue m -> m (NThunk m)
    buildThunk :: m (NValue m) -> m (NThunk m)
    forceThunk :: NThunk m -> m (NValue m)

    -- | Import a path into the nix store, and return the resulting path
    addPath :: FilePath -> m StorePath

deferInScope :: MonadNix m
             => NScopes m -> m (NValue m) -> m (NThunk m)
deferInScope scope = buildThunk . clearScopes . pushScopes scope

-- | MonadNixEnv represents all of the effects needed by builtin functions in
--   order to interact with the environment where Nix expressions are being
--   evaluated. They are only used by the functions defined in Builtins.hs.

class MonadNix m => MonadNixEnv m where
    importFile :: NThunk m -> m (NValue m)
    getEnvVar :: NThunk m -> m (NValue m)
