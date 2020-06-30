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
import qualified Data.List
import qualified Data.HashMap.Lazy
import           Data.Text                      (Text)
import qualified Data.Text
import qualified Data.Text.IO
import           Data.Text.Prettyprint.Doc      (Doc, (<+>))
import qualified Data.Text.Prettyprint.Doc
import qualified Data.Text.Prettyprint.Doc.Render.Text
import           Data.Version                   ( showVersion )
import           Paths_hnix                     ( version )

import           Control.Monad.Catch
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State.Strict

import           System.Console.Repline         ( Cmd
                                                , CompletionFunc
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

data IState t f m = IState
  { replIt  :: Maybe NExprLoc          -- ^ Last expression entered
  , replCtx :: AttrSet (NValue t f m)  -- ^ Value environment
  , replDbg :: Bool                    -- ^ Enable REPL debug output, dumping IState on each command
  } deriving (Eq, Show)

initState :: MonadIO m => IState t f m
initState = IState Nothing mempty False

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

  when (replDbg st) $ liftIO $ print st

  -- Parser ( returns AST as `NExprLoc` )
  case parseExprOrBinding source of
    (Failure err, _) -> do
      liftIO $ print err
      return Nothing
    (Success expr, isBinding) -> do

      -- Type Inference ( returns Typing Environment )
      --
      --let tyctx' = inferTop Env.empty [("repl", stripAnnotation expr)]
      --liftIO $ print tyctx'

      mVal <- lift $ lift $ try $ pushScope (replCtx st) (evalExprLoc expr)

      case mVal of
        Left (NixException frames) -> do
          lift $ lift $ liftIO . print =<< renderFrames @(NValue t f m) @t frames
          return Nothing
        Right val -> do
          -- Update the interpreter state
          when (update && isBinding) $ do
            -- Set `replIt` to last entered expression
            put st { replIt = Just expr }

            -- If the result value is a set, update our context with it
            case val of
              NVSet xs _ -> put st { replCtx = Data.HashMap.Lazy.union xs (replCtx st) }
              _          -> return ()

          return $ Just val
  where
    -- If parsing fails, turn the input into singleton attribute set
    -- and try again.
    --
    -- This allows us to handle assignments like @a = 42@
    -- which get turned into @{ a = 42; }@
    parseExprOrBinding i =
      case parseNixTextLoc i of
        Success expr -> (Success expr, False)
        Failure e    ->
          case parseNixTextLoc $ toAttrSet i of
            Failure _  -> (Failure e, False) -- return the first parsing failure
            Success e' -> (Success e', True)

    toAttrSet i = "{" <> i <> (if Data.Text.isSuffixOf ";" i then mempty else ";") <> "}"

cmd
  :: (MonadNix e t f m, MonadIO m)
  => String
  -> Repl e t f m ()
cmd source = do
  mVal <- exec True (Data.Text.pack source)
  case mVal of
    Nothing -> return ()
    Just val -> printValue val

printValue :: (MonadNix e t f m, MonadIO m)
           => NValue t f m
           -> Repl e t f m ()
printValue val = lift $ lift $ do
  opts :: Nix.Options <- asks (view hasLens)
  if
    | strict opts -> liftIO . print . prettyNValue =<< normalForm val
    | values opts -> liftIO . print . prettyNValueProv =<< removeEffects val
    | otherwise   -> liftIO . print . prettyNValue =<< removeEffects val

-------------------------------------------------------------------------------
-- Commands
-------------------------------------------------------------------------------

-- :browse command
browse :: (MonadNix e t f m, MonadIO m)
       => String
       -> Repl e t f m ()
browse _ = do
  st <- get
  forM_ (Data.HashMap.Lazy.toList $ replCtx st) $ \(k, v) -> do
    liftIO $ putStr $ Data.Text.unpack $ k <> " = "
    printValue v

-- :load command
load
  :: (MonadNix e t f m, MonadIO m)
  => String
  -> Repl e t f m ()
load args = do
  contents <- liftIO
    $ Data.Text.IO.readFile
    $ Data.Text.unpack
    $ Data.Text.strip
    $ Data.Text.pack args
  void $ exec True contents

-- :type command
typeof
  :: (MonadNix e t f m, MonadIO m)
  => String
  -> Repl e t f m ()
typeof args = do
  st <- get
  mVal <- case Data.HashMap.Lazy.lookup line (replCtx st) of
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

-- :debug command
debug :: (MonadNix e t f m, MonadIO m) => a -> Repl e t f m ()
debug _ = modify (\x -> x { replDbg = True })

-------------------------------------------------------------------------------
-- Interactive Shell
-------------------------------------------------------------------------------

-- Prefix tab completer
defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher =
  [ (":load", System.Console.Repline.fileCompleter)
  --, (":type"  , values)
  ]

-- Default tab completer
comp :: Monad m => WordCompleter m
comp n = do
  let cmds = [":load", ":type", ":browse", ":quit"]
  -- Env.TypeEnv ctx <- gets tyctx
  -- let defs = map Data.Text.unpack $ Map.keys ctx
  return $ filter (Data.List.isPrefixOf n) (cmds
    -- ++ defs
    )

-- HelpOption inspired by Dhall Repl
-- with `Doc` instead of String for syntax and doc
data HelpOption e t f m = HelpOption
  { helpOptionName     :: String
  , helpOptionSyntax   :: Doc ()
  , helpOptionDoc      :: Doc ()
  , helpOptionFunction :: Cmd (Repl e t f m)
  }

type HelpOptions e t f m = [HelpOption e t f m]

helpOptions :: (MonadNix e t f m, MonadIO m) => HelpOptions e t f m
helpOptions =
  [ HelpOption
      "help"
      ""
      "Print help text"
      (help helpOptions)
  , HelpOption
      "paste"
      ""
      "Enter multi-line mode"
      (error "Unreachable")
  , HelpOption
      "load"
      "FILENAME"
      "Load .nix file into scope"
      load
  , HelpOption
      "browse"
      ""
      "Browse bindings in interpreter context"
      browse
  , HelpOption
      "type"
      "EXPRESSION"
      "Evaluate expression or binding from context and print the type of the result value"
      typeof
  , HelpOption
      "quit"
      ""
      "Quit interpreter"
      quit
  , HelpOption
      "debug"
      ""
      "Enable REPL debugging output"
      debug
  ]

help :: (MonadNix e t f m, MonadIO m)
     => HelpOptions e t f m
     -> String
     -> Repl e t f m ()
help hs _ = do
  liftIO $ putStrLn "Available commands:\n"
  forM_ hs $ \h ->
      liftIO
    . Data.Text.IO.putStrLn
    . Data.Text.Prettyprint.Doc.Render.Text.renderStrict
    . Data.Text.Prettyprint.Doc.layoutPretty
        Data.Text.Prettyprint.Doc.defaultLayoutOptions
    $     ":"
       <>  Data.Text.Prettyprint.Doc.pretty (helpOptionName h)
       <+> helpOptionSyntax h
       <>  Data.Text.Prettyprint.Doc.line
       <>  Data.Text.Prettyprint.Doc.indent 4 (helpOptionDoc h)

options
  :: (MonadNix e t f m, MonadIO m)
  => System.Console.Repline.Options (Repl e t f m)
options = (\h -> (helpOptionName h, helpOptionFunction h)) <$> helpOptions

completer
  :: (MonadNix e t f m, MonadIO m)
  => CompleterStyle (StateT (IState t f m) m)
completer = Prefix (System.Console.Repline.wordCompleter comp) defaultMatcher
