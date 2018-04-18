{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Nix.Stack where

import           Control.Exception
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Data.ByteString (ByteString)
import           Data.Fix
import           Data.Functor.Compose
import           Data.List (intercalate)
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import           Data.Void
import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated
import           Nix.Options
import           Nix.Parser.Library
import           Nix.Pretty
import           Nix.Utils

newtype NixException = NixEvalException String
    deriving Show

instance Exception NixException

type Frames = [Either String NExprLoc]

type Framed e m = (MonadReader e m, Has e Frames, Has e Options, MonadThrow m)

withExprContext :: Framed e m => NExprLoc -> m r -> m r
withExprContext expr = local (over hasLens (Right @String expr :))

withStringContext :: Framed e m => String -> m r -> m r
withStringContext str = local (over hasLens (Left @_ @NExprLoc str :))

class Monad m => MonadFile m where
    readFile :: FilePath -> m ByteString

posAndMsg :: Options -> SourcePos -> Doc -> ParseError t Void
posAndMsg opts beg msg =
    FancyError (beg :| [])
        (Set.fromList [ErrorFail
                           (if verbose opts >= Chatty
                            then "While evaluating:\n>>>>>>>>\n"
                               ++ intercalate "  \n" (lines (show msg))
                               ++ "\n<<<<<<<<"
                            else "Expression: " ++ show msg)
                           :: ErrorFancy Void])

renderLocation :: (Framed e m, MonadFile m) => SrcSpan -> Doc -> m Doc
renderLocation (SrcSpan beg@(SourcePos "<string>" _ _) _) msg = do
    opts :: Options <- asks (view hasLens)
    return $ text $ parseErrorPretty @Char (posAndMsg opts beg msg)

renderLocation (SrcSpan beg@(SourcePos path _ _) _) msg = do
    opts :: Options <- asks (view hasLens)
    contents <- Nix.Stack.readFile path
    return $ text $ parseErrorPretty' contents (posAndMsg opts beg msg)

renderFrame :: (Framed e m, MonadFile m)
            => Either String NExprLoc -> m String
renderFrame (Left str) = return str
renderFrame (Right expr@(Fix (Compose (Ann ann x)))) = do
    opts :: Options <- asks (view hasLens)
    fmap show $ renderLocation ann $ prettyNix $
        if verbose opts >= Chatty
        then stripAnnotation expr
        else Fix (Fix (NSym "<?>") <$ x)

throwError :: (Framed e m, MonadFile m, MonadThrow m) => String -> m a
throwError str = do
    opts :: Options <- asks (view hasLens)
    context <- asks (reverse . view hasLens)
    infos   <- case context of
        [] -> return []
        _ | verbose opts >= Talkative ->
            mapM renderFrame $
                filter noAsserts (init context) ++ [last context]
          | verbose opts >= Informational ->
            return [sourcePosStackPretty
                        (NE.fromList (concatMap justPos (reverse context)))]
          | otherwise ->
            return []
    traceM "throwing error"
    throwM $ NixEvalException $ intercalate "\n" $ infos ++ [str]
  where
    noAsserts (Right (Fix (Compose (Ann _ (NAssert _ _))))) = False
    noAsserts _ = True

    justPos (Left _) = []
    justPos (Right (Fix (Compose (Ann (SrcSpan beg _) _)))) = [beg]
