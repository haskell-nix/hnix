{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Nix.Trace where

import           Control.Applicative
import           Control.Arrow (second)
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Fix
import           Data.Functor.Compose
import           Data.IORef
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Maybe (maybe, fromMaybe, catMaybes, mapMaybe)
import           Data.Text (Text)
import           Nix.Atoms
import           Nix.Exec
import           Nix.Expr
import           Nix.Parser
import           Nix.Stack
import           Nix.Utils
import           System.Directory
import           System.FilePath
import           Text.Megaparsec.Pos

processImports :: Maybe FilePath
               -> NExprLoc
               -> StateT (HashMap FilePath NExprLoc) IO NExprLoc
processImports mfile expr = do
    imports <- get
    flip cataM expr $ \case
        Compose
            (Ann _ (NBinary NApp
                       (Fix (Compose (Ann _ (NSym "import"))))
                       (Fix (Compose (Ann _ (NLiteralPath origPath))))))
            | Just expr <- M.lookup origPath imports -> pure expr
            | otherwise -> do
                traceM $ "Importing mfile " ++ show mfile
                traceM $ "Importing origPath " ++ origPath
                path <- liftIO $ pathToDefaultNixFile origPath
                traceM $ "Importing path " ++ path
                path' <- liftIO $ pathToDefaultNixFile =<< canonicalizePath
                    (maybe path (\p -> takeDirectory p </> path) mfile)
                traceM $ "Importing file " ++ path'

                eres <- liftIO $ parseNixFileLoc path'
                case eres of
                    Failure err  -> error $ "Parse failed: " ++ show err
                    Success x -> do
                        let pos  = SourcePos "Trace.hs" (mkPos 1) (mkPos 1)
                            span = SrcSpan pos pos
                            cur  = NamedVar
                                (StaticKey "__cur_file" (Just pos) :| [])
                                (Fix (Compose (Ann span (NLiteralPath path'))))
                            x'   = Fix (Compose
                                        (Ann span (NLet [cur] x)))
                        modify (M.insert origPath x')
                        processImports (Just path') x'
        x -> pure $ Fix x

newtype FlaggedF (f :: * -> *) r = FlaggedF { flagged :: (IORef Bool, f r) }
    deriving (Functor, Foldable, Traversable)

instance Show (f r) => Show (FlaggedF f r) where
    show (FlaggedF (_, x)) = show x

-- instance Show (f r) => Show (FlaggedF f r) where
--     show (FlaggedF (b, x)) =
--         let !used = unsafePerformIO (readIORef b) in
--         if used
--         then show x
--         else "<<" ++ show x ++ ">>"

type Flagged (f :: * -> *) = Fix (FlaggedF f)

flagExprLoc :: MonadIO n => NExprLoc -> n (Flagged NExprLocF)
flagExprLoc = cataM $ \x -> do
    flag <- liftIO $ newIORef False
    pure $ Fix $ FlaggedF (flag, x)

stripFlags :: Flagged NExprLocF -> NExprLoc
stripFlags = cata $ \(FlaggedF (_, x)) -> Fix x

pruneTree :: MonadIO n => Flagged NExprLocF -> n (Maybe NExprLoc)
pruneTree = cataM $ \(FlaggedF (b, Compose x)) -> do
    used <- liftIO $ readIORef b
    pure $ if used
           then Fix . Compose <$> traverse prune x
           else Nothing
  where
    prune :: NExprF (Maybe NExprLoc) -> Maybe (NExprF NExprLoc)
    prune = \case
        NStr str                  -> Just $ NStr (pruneString str)
        NHasAttr (Just aset) attr -> Just $ NHasAttr aset (NE.map pruneKeyName attr)
        NAbs params (Just body)   -> Just $ NAbs (pruneParams params) body

        NBinary op Nothing (Just rarg) -> Just $ NBinary op nNull rarg
        NBinary op (Just larg) Nothing -> Just $ NBinary op larg nNull

        NList l -> case catMaybes l of
            [] -> Nothing
            xs -> Just $ NList xs

        NSet binds    -> case mapMaybe pruneBinding binds of
            [] -> Nothing
            xs -> Just $ NSet xs
        NRecSet binds -> case mapMaybe pruneBinding binds of
            [] -> Nothing
            xs -> Just $ NRecSet xs

        NLet binds (Just body@(Fix (Compose (Ann _ x)))) ->
            Just $ case mapMaybe pruneBinding binds of
                [] -> x
                xs -> NLet xs body

        NSelect (Just aset) attr alt ->
            Just $ NSelect aset (NE.map pruneKeyName attr) (join alt)

        -- These are the only short-circuiting binary operators
        NBinary NAnd (Just (Fix (Compose (Ann _ larg)))) _ -> Just larg
        NBinary NOr  (Just (Fix (Compose (Ann _ larg)))) _ -> Just larg

        -- If the scope of a with was never referenced, it's not needed
        NWith Nothing (Just (Fix (Compose (Ann _ body)))) -> Just body

        NAssert Nothing _ ->
            error "How can an assert be used, but its condition not?"

        NAssert _ (Just (Fix (Compose (Ann _ body)))) -> Just body
        NAssert (Just cond) _ -> Just $ NAssert cond nNull

        NIf Nothing _ _ ->
            error "How can an if be used, but its condition not?"

        NIf _ Nothing (Just (Fix (Compose (Ann _ f)))) -> Just f
        NIf _ (Just (Fix (Compose (Ann _ t)))) Nothing -> Just t

        x -> sequence x

    pruneString :: NString (Maybe NExprLoc) -> NString NExprLoc
    pruneString (DoubleQuoted xs) =
        DoubleQuoted (mapMaybe pruneAntiquotedText xs)
    pruneString (Indented n xs)   =
        Indented n (mapMaybe pruneAntiquotedText xs)

    pruneAntiquotedText
        :: Antiquoted Text (Maybe NExprLoc)
        -> Maybe (Antiquoted Text NExprLoc)
    pruneAntiquotedText (Plain v)             = Just (Plain v)
    pruneAntiquotedText EscapedNewline        = Just EscapedNewline
    pruneAntiquotedText (Antiquoted Nothing)  = Nothing
    pruneAntiquotedText (Antiquoted (Just k)) = Just (Antiquoted k)

    pruneAntiquoted
        :: Antiquoted (NString (Maybe NExprLoc)) (Maybe NExprLoc)
        -> Maybe (Antiquoted (NString NExprLoc) NExprLoc)
    pruneAntiquoted (Plain v)             = Just (Plain (pruneString v))
    pruneAntiquoted EscapedNewline        = Just EscapedNewline
    pruneAntiquoted (Antiquoted Nothing)  = Nothing
    pruneAntiquoted (Antiquoted (Just k)) = Just (Antiquoted k)

    pruneKeyName :: NKeyName (Maybe NExprLoc) -> NKeyName NExprLoc
    pruneKeyName (StaticKey n p) = StaticKey n p
    pruneKeyName (DynamicKey k)
        | Just k' <- pruneAntiquoted k = DynamicKey k'
        | otherwise = StaticKey "unused" Nothing

    pruneParams :: Params (Maybe NExprLoc) -> Params NExprLoc
    pruneParams (Param n) = Param n
    pruneParams (ParamSet xs b n) = ParamSet (map (second join) xs) b n

    pruneBinding :: Binding (Maybe NExprLoc) -> Maybe (Binding NExprLoc)
    pruneBinding (NamedVar _ Nothing)  = Nothing
    pruneBinding (NamedVar xs (Just x)) =
        Just (NamedVar (NE.map pruneKeyName xs) x)
    pruneBinding (Inherit _ [])  = Nothing
    pruneBinding (Inherit m xs)  =
        Just (Inherit (join m) (map pruneKeyName xs))

nNull :: NExprLoc
nNull = Fix (Compose (Ann (SrcSpan nullPos nullPos) (NConstant NNull)))
  where
    nullPos = SourcePos "<unknown>" (mkPos 0) (mkPos 0)

tracingEvalExpr :: (Framed e m, Exception r, MonadCatch m, MonadIO m,
                   MonadCatch n, MonadIO n, Alternative n)
                => (NExprF (m v) -> m v) -> Maybe FilePath -> NExprLoc
                -> n (m (NExprLoc, Either r v))
tracingEvalExpr eval mpath expr = do
    expr' <- flagExprLoc
        =<< liftIO (evalStateT (processImports mpath expr) M.empty)
    res <- flip runReaderT (0 :: Int) $
        adiM (pure <$> eval . annotated . getCompose . snd . flagged)
             psi expr'
    return $ do
        eres   <- catch (Right <$> res) (pure . Left)
        expr'' <- pruneTree expr'
        return (fromMaybe nNull expr'', eres)
  where
    psi k v@(Fix (FlaggedF (b, _x))) = do
        depth <- ask
        guard (depth < 200)
        local succ $ do
            action <- k v
            return $ withExprContext (stripFlags v) $ do
                traceM $ "eval: " ++ replicate depth ' '
                    ++ show (void (unFix (stripAnnotation (stripFlags v))))
                liftIO $ writeIORef b True
                res <- action
                traceM $ "eval: " ++ replicate depth ' ' ++ "."
                return res
