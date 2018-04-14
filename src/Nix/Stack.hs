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
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Set as Set
import           Data.Void
import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated
import           Nix.Parser.Library
import           Nix.Pretty
import           Nix.Utils

newtype NixException = NixEvalException String
    deriving Show

instance Exception NixException

type Frames = [Either String (NExprLocF ())]

type Framed e m = (MonadReader e m, Has e Frames, MonadThrow m)

withExprContext :: Framed e m => NExprLocF () -> m r -> m r
withExprContext expr = local (over hasLens (Right @String expr :))

withStringContext :: Framed e m => String -> m r -> m r
withStringContext str = local (over hasLens (Left @_ @(NExprLocF ()) str :))

class Monad m => MonadFile m where
    readFile :: FilePath -> m ByteString

renderLocation :: MonadFile m => SrcSpan -> Doc -> m Doc
renderLocation (SrcSpan beg@(SourcePos "<string>" _ _) _end) msg =
    return $ text $ parseErrorPretty @Char $
        FancyError (beg :| [])
            (Set.fromList [ErrorFail ("While evaluating: " ++ show msg)
                               :: ErrorFancy Void])
renderLocation (SrcSpan beg@(SourcePos path _ _) _end) msg = do
    contents <- Nix.Stack.readFile path
    return $ text $ parseErrorPretty' contents $
        FancyError (beg :| [])
            (Set.fromList [ErrorFail ("While evaluating: " ++ show msg)
                               :: ErrorFancy Void])

renderFrame :: MonadFile m => Either String (NExprLocF ()) -> m String
renderFrame (Left str) = return str
renderFrame (Right (Compose (Ann ann expr))) =
    show <$> renderLocation ann
        (prettyNix (Fix (Fix (NSym "<?>") <$ expr)))

throwError :: (Framed e m, MonadFile m, MonadThrow m) => String -> m a
throwError str = do
    context <- asks (reverse . view hasLens)
    infos   <- case context of
        [] -> return []
        _ -> mapM renderFrame $
                filter noAsserts (init context) ++ [last context]
    traceM "throwing error"
    throwM $ NixEvalException $ unlines $ infos ++ [str]
  where
    noAsserts (Right (Compose (Ann _ (NAssert _ _)))) = False
    noAsserts _ = True
