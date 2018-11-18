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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Repl where

import           Nix
import           Nix.Convert
import           Nix.Eval
import           Nix.Scope
import qualified Nix.Type.Env as Env
import           Nix.Type.Infer
import           Nix.Utils

import qualified Data.HashMap.Lazy as M
import           Data.List (isPrefixOf, foldl')
import qualified Data.Map as Map
import           Data.Monoid
import           Data.Text (unpack, pack)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State.Strict

import           System.Console.Haskeline.MonadException
import           System.Console.Repline
import           System.Environment
import           System.Exit

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

newtype IState m = IState
  { tmctx :: AttrSet (NValue m)  -- Value environment
  }

initState :: MonadIO m => IState m
initState = IState M.empty

type Repl e m a = HaskelineT (StateT (IState m) m) a
hoistErr :: MonadIO m => Result a -> Repl e m a
hoistErr (Success val) = return val
hoistErr (Failure err) = do
  liftIO $ print err
  abort

-------------------------------------------------------------------------------
-- Execution
-------------------------------------------------------------------------------

exec :: forall e m. (MonadNix e m, MonadIO m, MonadException m)
     => Bool -> Text.Text -> Repl e m (NValue m)
exec update source = do
  -- Get the current interpreter state
  st <- get

  -- Parser ( returns AST )
  -- TODO: parse <var> = <expr>
  expr <- hoistErr $ parseNixTextLoc source

  -- Type Inference ( returns Typing Environment )
  -- tyctx' <- hoistErr $ inferTop (tyctx st) expr

  mVal <- liftIO $ pushScope (tmctx st) (try $ evalExprLoc expr)

  case mVal of
    Left (NixException frames) -> do
      liftIO . print =<< renderFrames @(NThunk m) frames
      abort
    Right val -> do
      -- Update the interpreter state
      when update $ do
        -- Create the new environment
        put st { tmctx = tmctx st -- TODO: M.insert key val (tmctx st)
               }
      return val


cmd :: (MonadNix e m, MonadIO m, MonadException m) => String -> Repl e m ()
cmd source = do
  val <- exec True (Text.pack source)
  lift $ lift $ do
    opts :: Nix.Options <- asks (view hasLens)
    if | strict opts ->
         liftIO . print . prettyNValueNF =<< normalForm val
       | values opts ->
         liftIO . print =<< prettyNValueProv val
       | otherwise ->
         liftIO . print =<< prettyNValue val
-------------------------------------------------------------------------------
-- Commands
-------------------------------------------------------------------------------

-- :browse command
browse :: MonadNix e m => [String] -> Repl e m ()
browse _ = do
  st <- get
  undefined
  -- liftIO $ mapM_ putStrLn $ ppenv (tyctx st)

-- :load command
load :: (MonadNix e m, MonadIO m, MonadException m) => [String] -> Repl e m ()
load args = do
  contents <- liftIO $ Text.readFile (unwords args)
  void $ exec True contents

-- :type command
typeof :: (MonadNix e m, MonadException m, MonadIO m) => [String] -> Repl e m ()
typeof args = do
  st <- get
  let arg = unwords args
  val <- case M.lookup (Text.pack arg) (tmctx st) of
    Just val -> return val
    Nothing -> exec False (Text.pack arg)
  liftIO $ putStrLn $ describeValue $ valueType (_baseValue val)

-- :quit command
quit :: (MonadNix e m, MonadIO m) => a -> Repl e m ()
quit _ = liftIO exitSuccess

-------------------------------------------------------------------------------
-- Interactive Shell
-------------------------------------------------------------------------------

-- Prefix tab completer
defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = [
    (":load"  , fileCompleter)
  --, (":type"  , values)
  ]

-- Default tab completer
comp :: Monad m => WordCompleter m
comp n = do
  let cmds = [":load", ":type", ":browse", ":quit"]
  -- Env.TypeEnv ctx <- gets tyctx
  -- let defs = map unpack $ Map.keys ctx
  return $ filter (isPrefixOf n) (cmds {-++ defs-})

options :: (MonadNix e m, MonadIO m, MonadException m)
        => [(String, [String] -> Repl e m ())]
options = [
    ("load"   , load)
  , ("browse" , browse)
  , ("quit"   , quit)
  , ("type"   , typeof)
  ]

-------------------------------------------------------------------------------
-- Entry Point
-------------------------------------------------------------------------------

completer :: (MonadNix e m, MonadIO m) => CompleterStyle (StateT (IState m) m)
completer = Prefix (wordCompleter comp) defaultMatcher

shell :: (MonadNix e m, MonadIO m, MonadException m) => Repl e m a -> m ()
shell pre = flip evalStateT initState $
    evalRepl "hnix> " cmd options completer pre

-------------------------------------------------------------------------------
-- Toplevel
-------------------------------------------------------------------------------

-- main :: IO ()
-- main = do
--   args <- getArgs
--   case args of
--     []      -> shell (return ())
--     [fname] -> shell (load [fname])
--     ["test", fname] -> shell (load [fname] >> browse [] >> quit ())
--     _ -> putStrLn "invalid arguments"
