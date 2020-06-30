{- This code was authored by:

     Stephen Diehl
     Kwang Yul Seo <kwangyul.seo@gmail.com>

   It was made available under the MIT license. See the src/Nix/Type
   directory for more details.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
import           Data.Text                     (Text)
import qualified Data.Text
import qualified Data.Text.IO
import           Data.Version                   ( showVersion )
import           Paths_hnix                     ( version )

import           Control.Monad.Catch
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State.Strict

import           System.Console.Repline         ( CompletionFunc
                                                , CompleterStyle (Prefix)
                                                , ExitDecision(Exit)
                                                , HaskelineT
                                                , WordCompleter
                                                )
import qualified System.Console.Repline
import qualified System.Exit


main :: (MonadNix e t f m, MonadIO m, MonadMask m) => m ()
main = flip evalStateT initState
    $ System.Console.Repline.evalRepl
        banner
        cmd
        options
        (Just ':')
        (Just "paste")
        completer
        greeter
        finalizer
 where
  banner = pure . \case
    System.Console.Repline.SingleLine -> "hnix> "
    System.Console.Repline.MultiLine  -> "| "
  greeter =
    liftIO
      $  putStrLn
      $  "Welcome to hnix "
      <> showVersion version
      <> ". For help type :help\n"
  finalizer = do
    liftIO $ putStrLn "Goodbye."
    return Exit

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

newtype IState t f m = IState
  { tmctx :: AttrSet (NValue t f m)  -- Value environment
  }

initState :: MonadIO m => IState t f m
initState = IState mempty

type Repl e t f m = HaskelineT (StateT (IState t f m) m)

-------------------------------------------------------------------------------
-- Execution
-------------------------------------------------------------------------------

exec
  :: forall e t f m
   . (MonadNix e t f m, MonadIO m)
  => Bool
  -> Text
  -> Repl e t f m (Maybe (NValue t f m))
exec update source = do
  -- Get the current interpreter state
  st <- get

  -- Parser ( returns AST )
  -- TODO: parse <var> = <expr>
  case parseNixTextLoc source of
    Failure err -> do
      liftIO $ print err
      return Nothing
    Success expr -> do
      -- Type Inference ( returns Typing Environment )
      --let tyctx' = inferTop Env.empty [("repl", stripAnnotation expr)]
      --liftIO $ print tyctx'

      -- TODO: track scope with (tmctx st)
      mVal <- lift $ lift $ try $ pushScope mempty (evalExprLoc expr)

      case mVal of
        Left (NixException frames) -> do
          lift $ lift $ liftIO . print =<< renderFrames @(NValue t f m) @t frames
          return Nothing
        Right val -> do
          -- Update the interpreter state
          when update $ do
            -- Create the new environment
            put st { tmctx = tmctx st } -- TODO: M.insert key val (tmctx st)
          return $ Just val

cmd
  :: (MonadNix e t f m, MonadIO m)
  => String
  -> Repl e t f m ()
cmd source = do
  mVal <- exec True (Data.Text.pack source)
  case mVal of
    Nothing -> return ()
    Just val -> do
      lift $ lift $ do
        opts :: Nix.Options <- asks (view hasLens)
        if
          | strict opts -> liftIO . print . prettyNValue =<< normalForm val
          | values opts -> liftIO . print . prettyNValueProv =<< removeEffects val
          | otherwise   -> liftIO . print . prettyNValue =<< removeEffects val

-------------------------------------------------------------------------------
-- Commands
-------------------------------------------------------------------------------

-- :browse command
browse :: MonadNix e t f m => String -> Repl e t f m ()
browse _ = do
  st <- get
  undefined
  -- liftIO $ mapM_ putStrLn $ ppenv (tyctx st)

-- :load command
load
  :: (MonadNix e t f m, MonadIO m)
  => String
  -> Repl e t f m ()
load args = do
  contents <- liftIO $ Data.Text.IO.readFile args
  void $ exec True contents

-- :type command
typeof
  :: (MonadNix e t f m, MonadIO m)
  => String
  -> Repl e t f m ()
typeof args = do
  st <- get
  mVal <- case M.lookup line (tmctx st) of
    Just val -> return $ Just val
    Nothing  -> do
      exec False line

  forM_ mVal $ \val -> do
    s <- lift . lift . showValueType $ val
    liftIO $ putStrLn s

  where line = Data.Text.pack args

-- :quit command
quit :: (MonadNix e t f m, MonadIO m) => a -> Repl e t f m ()
quit _ = liftIO System.Exit.exitSuccess

-------------------------------------------------------------------------------
-- Interactive Shell
-------------------------------------------------------------------------------

-- Prefix tab completer
defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher =
  [(":load", System.Console.Repline.fileCompleter)
  --, (":type"  , values)
                           ]
-- Default tab completer
comp :: Monad m => WordCompleter m
comp n = do
  let cmds = [":load", ":type", ":browse", ":quit"]
  -- Env.TypeEnv ctx <- gets tyctx
  -- let defs = map Data.Text.unpack $ Map.keys ctx
  return $ filter (isPrefixOf n) (cmds
    -- ++ defs
    )

options
  :: (MonadNix e t f m, MonadIO m)
  => System.Console.Repline.Options (Repl e t f m)
options =
  [ ( "load" , load)
  --, ("browse" , browse)
  , ("quit", quit)
  , ("type", typeof)
  , ("help", help)
  ]

help
  :: forall e t f m
   . (MonadNix e t f m, MonadIO m)
  => String
  -> Repl e t f m ()
help _ = liftIO $ do
  putStrLn "Available commands:\n"
  mapM_ putStrLn $ map (\o -> ":" ++ (fst o)) (options @e @t @f @m)
  putStrLn ":paste - enter multi-line mode"

completer
  :: (MonadNix e t f m, MonadIO m)
  => CompleterStyle (StateT (IState t f m) m)
completer = Prefix (System.Console.Repline.wordCompleter comp) defaultMatcher
