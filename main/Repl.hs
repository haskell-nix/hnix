{- This code was authored by:

     Stephen Diehl
     Kwang Yul Seo <kwangyul.seo@gmail.com>

   It was made available under the MIT license. See the src/Nix/Type
   directory for more details.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Repl where

import           Nix                     hiding ( exec
                                                , try
                                                )
import           Nix.Cited
import           Nix.Convert
import           Nix.Eval
import           Nix.Scope
import qualified Nix.Type.Env                  as Env
import           Nix.Type.Infer
import           Nix.Utils

import           Control.Comonad
import qualified Data.HashMap.Lazy             as M
import           Data.List                      ( isPrefixOf
                                                , foldl'
                                                )
import qualified Data.Map                      as Map
import           Data.Monoid
import           Data.Text                      ( unpack
                                                , pack
                                                )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Data.Version                   ( showVersion )
import           Paths_hnix                     ( version )

import           Control.Monad.Catch
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State.Strict

import           System.Console.Haskeline.MonadException
import           System.Console.Repline
import           System.Environment
import           System.Exit


main :: (MonadNix e t f m, MonadIO m, MonadException m) => m ()
main = flip evalStateT initState
#if MIN_VERSION_repline(0, 2, 0)
    $ evalRepl (return prefix) cmd options (Just ':') completer welcomeText
#else
    $ evalRepl prefix cmd options completer welcomeText
#endif
 where
  prefix = "hnix> "
  welcomeText =
    liftIO
      $  putStrLn
      $  "Welcome to hnix "
      <> showVersion version
      <> ". For help type :help\n"

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

newtype IState t f m = IState
  { tmctx :: AttrSet (NValue t f m)  -- Value environment
  }

initState :: MonadIO m => IState t f m
initState = IState M.empty

type Repl e t f m = HaskelineT (StateT (IState t f m) m)
hoistErr :: MonadIO m => Result a -> Repl e t f m a
hoistErr (Success val) = return val
hoistErr (Failure err) = do
  liftIO $ print err
  abort

-------------------------------------------------------------------------------
-- Execution
-------------------------------------------------------------------------------

exec
  :: forall e t f m
   . (MonadNix e t f m, MonadIO m, MonadException m)
  => Bool
  -> Text.Text
  -> Repl e t f m (NValue t f m)
exec update source = do
  -- Get the current interpreter state
  st   <- get

  -- Parser ( returns AST )
  -- TODO: parse <var> = <expr>
  expr <- hoistErr $ parseNixTextLoc source

  -- Type Inference ( returns Typing Environment )
  -- tyctx' <- hoistErr $ inferTop (tyctx st) expr

  -- TODO: track scope with (tmctx st)
  mVal <- lift $ lift $ try $ pushScope M.empty (evalExprLoc expr)

  case mVal of
    Left (NixException frames) -> do
      lift $ lift $ liftIO . print =<< renderFrames @(NValue t f m) @t frames
      abort
    Right val -> do
      -- Update the interpreter state
      when update $ do
        -- Create the new environment
        put st { tmctx = tmctx st } -- TODO: M.insert key val (tmctx st)
      return val


cmd
  :: (MonadNix e t f m, MonadIO m, MonadException m)
  => String
  -> Repl e t f m ()
cmd source = do
  val <- exec True (Text.pack source)
  lift $ lift $ do
    opts :: Nix.Options <- asks (view hasLens)
    if
      | strict opts -> liftIO . print . prettyNValueNF =<< normalForm val
      | values opts -> liftIO . print =<< prettyNValueProv val
      | otherwise   -> liftIO . print =<< prettyNValue val
-------------------------------------------------------------------------------
-- Commands
-------------------------------------------------------------------------------

-- :browse command
browse :: MonadNix e t f m => [String] -> Repl e t f m ()
browse _ = do
  st <- get
  undefined
  -- liftIO $ mapM_ putStrLn $ ppenv (tyctx st)

-- :load command
load
  :: (MonadNix e t f m, MonadIO m, MonadException m)
  => [String]
  -> Repl e t f m ()
load args = do
  contents <- liftIO $ Text.readFile (unwords args)
  void $ exec True contents

-- :type command
typeof
  :: (MonadNix e t f m, MonadException m, MonadIO m)
  => [String]
  -> Repl e t f m ()
typeof args = do
  st  <- get
  val <- case M.lookup line (tmctx st) of
    Just val -> return val
    Nothing  -> exec False line
  str <- lift $ lift $ showValueType val
  liftIO $ putStrLn str
  where line = Text.pack (unwords args)

-- :quit command
quit :: (MonadNix e t f m, MonadIO m) => a -> Repl e t f m ()
quit _ = liftIO exitSuccess

-------------------------------------------------------------------------------
-- Interactive Shell
-------------------------------------------------------------------------------

-- Prefix tab completer
defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher =
  [(":load", fileCompleter)
  --, (":type"  , values)
                           ]

-- Default tab completer
comp :: Monad m => WordCompleter m
comp n = do
  let cmds = [":load", ":type", ":browse", ":quit"]
  -- Env.TypeEnv ctx <- gets tyctx
  -- let defs = map unpack $ Map.keys ctx
  return $ filter (isPrefixOf n) (cmds {-++ defs-}
                                      )

options
  :: (MonadNix e t f m, MonadIO m, MonadException m)
  => [(String, [String] -> Repl e t f m ())]
options =
  [ ( "load"
    , load
    )
  --, ("browse" , browse)
  , ("quit", quit)
  , ("type", typeof)
  , ("help", help)
  ]

help
  :: forall e t f m
   . (MonadNix e t f m, MonadIO m, MonadException m)
  => [String]
  -> Repl e t f m ()
help _ = liftIO $ do
  putStrLn "Available commands:\n"
  mapM_ putStrLn $ map (\o -> ":" ++ (fst o)) (options @e @t @f @m)

completer
  :: (MonadNix e t f m, MonadIO m)
  => CompleterStyle (StateT (IState t f m) m)
completer = Prefix (wordCompleter comp) defaultMatcher



