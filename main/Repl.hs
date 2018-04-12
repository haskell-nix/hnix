{- This code was authored by:

     Stephen Diehl
     Kwang Yul Seo <kwangyul.seo@gmail.com>

   It was made available under the MIT license. See the src/Nix/Type
   directory for more details.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Repl where

import           Nix.Type.Infer
import qualified Nix.Type.Env as Env

import qualified Data.Map as Map
import           Data.Monoid
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import           Control.Monad.Identity
import           Control.Monad.State.Strict

import           Data.List (isPrefixOf, foldl')
import           Data.Text (unpack, pack)
import qualified Data.Text as Text

import           System.Exit
import           System.Environment
import           System.Console.Repline

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data TermEnv

data IState = IState
  { tyctx :: Env.Env  -- Type environment
  , tmctx :: TermEnv  -- Value environment
  }

initState :: IState
initState = IState Env.empty undefined

type Repl a = HaskelineT (StateT IState IO) a
hoistErr :: Show e => Either e a -> Repl a
hoistErr (Right val) = return val
hoistErr (Left err) = do
  liftIO $ print err
  abort

-------------------------------------------------------------------------------
-- Execution
-------------------------------------------------------------------------------

exec :: Bool -> L.Text -> Repl ()
exec update source = do
  -- Get the current interpreter state
  st <- get

  -- Parser ( returns AST )
  -- mod <- hoistErr $ parseModule "<stdin>" source

  -- Type Inference ( returns Typing Environment )
  -- tyctx' <- hoistErr $ inferTop (tyctx st) mod

  -- Create the new environment
  -- let st' = st { tmctx = foldl' evalDef (tmctx st) mod
  --              , tyctx = tyctx' <> tyctx st
  --              }

  -- Update the interpreter state
  -- when update (put st')

  -- If a value is entered, print it.
  -- case lookup "it" mod of
  --   Nothing -> return ()
  --   Just ex -> do
  --     let (val, _) = runEval (tmctx st') "it"  ex
  --     showOutput (show val) st'

  undefined

showOutput :: String -> IState -> Repl ()
showOutput arg st =
  case Env.lookup "it" (tyctx st)  of
    Just val -> liftIO $ putStrLn $ undefined -- ppsignature (arg, val)
    Nothing -> return ()

cmd :: String -> Repl ()
cmd source = exec True (L.pack source)

-------------------------------------------------------------------------------
-- Commands
-------------------------------------------------------------------------------

-- :browse command
browse :: [String] -> Repl ()
browse _ = do
  st <- get
  undefined
  -- liftIO $ mapM_ putStrLn $ ppenv (tyctx st)

-- :load command
load :: [String] -> Repl ()
load args = do
  contents <- liftIO $ L.readFile (unwords args)
  exec True contents

-- :type command
typeof :: [String] -> Repl ()
typeof args = do
  st <- get
  let arg = unwords args
  case Env.lookup (pack arg) (tyctx st) of
    Just val -> liftIO $ putStrLn $ undefined -- ppsignature (arg, val)
    Nothing -> exec False (L.pack arg)

-- :quit command
quit :: a -> Repl ()
quit _ = liftIO $ exitSuccess

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
  Env.TypeEnv ctx <- gets tyctx
  let defs = map unpack $ Map.keys ctx
  return $ filter (isPrefixOf n) (cmds ++ defs)

options :: [(String, [String] -> Repl ())]
options = [
    ("load"   , load)
  , ("browse" , browse)
  , ("quit"   , quit)
  , ("type"   , Repl.typeof)
  ]

-------------------------------------------------------------------------------
-- Entry Point
-------------------------------------------------------------------------------

completer :: CompleterStyle (StateT IState IO)
completer = Prefix (wordCompleter comp) defaultMatcher

shell :: Repl a -> IO ()
shell pre = flip evalStateT initState
     $ evalRepl "Poly> " cmd options completer pre

-------------------------------------------------------------------------------
-- Toplevel
-------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> shell (return ())
    [fname] -> shell (load [fname])
    ["test", fname] -> shell (load [fname] >> browse [] >> quit ())
    _ -> putStrLn "invalid arguments"
