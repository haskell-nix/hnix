{- This code was authored by:

     Stephen Diehl
     Kwang Yul Seo <kwangyul.seo@gmail.com>

   It was made available under the MIT license. See the src/Nix/Type
   directory for more details.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Repl where

import           Nix
import           Nix.Eval
import           Nix.Scope
import qualified Nix.Type.Env as Env
import           Nix.Type.Infer

import qualified Data.HashMap.Lazy as M
import           Data.List (isPrefixOf, foldl')
import qualified Data.Map as Map
import           Data.Monoid
import           Data.Text (unpack, pack)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           Control.Monad.Identity
import           Control.Monad.State.Strict

import           System.Console.Haskeline.MonadException
import           System.Console.Repline
import           System.Environment
import           System.Exit

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data TermEnv

data IState = IState
  { -- tyctx :: Env.Env  -- Type environment
  -- ,
    tmctx :: TermEnv  -- Value environment
  }

initState :: IState
initState = IState {-Env.empty-} undefined

type Repl e m a = HaskelineT (StateT IState m) a
hoistErr :: MonadIO m => Result a -> Repl e m a
hoistErr (Success val) = return val
hoistErr (Failure err) = do
  liftIO $ print err
  abort

-------------------------------------------------------------------------------
-- Execution
-------------------------------------------------------------------------------

exec :: forall e m. (MonadNix e m, MonadIO m) => Bool -> Text.Text -> Repl e m ()
exec update source = do
  -- Get the current interpreter state
  st <- get

  -- Parser ( returns AST )
  expr <- hoistErr $ parseNixTextLoc source

  -- Type Inference ( returns Typing Environment )
  -- tyctx' <- hoistErr $ inferTop (tyctx st) expr

  -- Create the new environment
  let st' = st { tmctx = tmctx st -- foldl' evalDef (tmctx st) expr
               -- , tyctx = tyctx' <> tyctx st
               }

  -- Update the interpreter state
  when update (put st')

  -- If a value is entered, print it.
  val <-
      -- jww (2018-04-12): Once the user is able to establish definitions
      -- in the repl, they should be passed here.
      lift $ lift $ pushScope @(NThunk m) M.empty $ evalExprLoc expr
  liftIO $ print val

cmd :: (MonadNix e m, MonadIO m) => String -> Repl e m ()
cmd source = exec True (Text.pack source)

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
load :: (MonadNix e m, MonadIO m) => [String] -> Repl e m ()
load args = do
  contents <- liftIO $ Text.readFile (unwords args)
  exec True contents

-- :type command
-- typeof :: [String] -> Repl e m ()
-- typeof args = do
--   st <- get
--   let arg = unwords args
--   case Env.lookup (pack arg) (tyctx st) of
--     Just val -> liftIO $ putStrLn $ undefined -- ppsignature (arg, val)
--     Nothing -> exec False (Text.pack arg)

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
comp :: (Monad m, MonadState IState m) => WordCompleter m
comp n = do
  let cmds = [":load", ":type", ":browse", ":quit"]
  -- Env.TypeEnv ctx <- gets tyctx
  -- let defs = map unpack $ Map.keys ctx
  return $ filter (isPrefixOf n) (cmds {-++ defs-})

options :: (MonadNix e m, MonadIO m) => [(String, [String] -> Repl e m ())]
options = [
    ("load"   , load)
  , ("browse" , browse)
  , ("quit"   , quit)
  -- , ("type"   , Repl.typeof)
  ]

-------------------------------------------------------------------------------
-- Entry Point
-------------------------------------------------------------------------------

completer :: (MonadNix e m, MonadIO m) => CompleterStyle (StateT IState m)
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
