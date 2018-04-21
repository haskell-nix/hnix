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
import           Control.Monad.Reader
import           Data.Fix
import           Data.Functor.Compose
import           Data.IORef
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Text (Text)
import           Nix.Atoms
import           Nix.Exec (MonadNix)
import           Nix.Expr
import           Nix.Options
import           Nix.Pretty (prettyNix)
import           Nix.Reduce
import           Nix.Stack
import           Nix.Utils
import           Text.Megaparsec.Pos
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

newtype FlaggedF f r = FlaggedF { flagged :: (IORef Bool, f r) }
    deriving (Functor, Foldable, Traversable)

instance Show (f r) => Show (FlaggedF f r) where
    show (FlaggedF (_, x)) = show x

type Flagged f = Fix (FlaggedF f)

flagExprLoc :: (MonadIO n, Traversable f)
            => Fix f -> n (Flagged f)
flagExprLoc = cataM $ \x -> do
    flag <- liftIO $ newIORef False
    pure $ Fix $ FlaggedF (flag, x)

stripFlags :: Functor f => Flagged f -> Fix f
stripFlags = cata $ Fix . snd . flagged

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

        NList   l     -> Just $ NList   (map       (fromMaybe nNull)  l)
        NSet    binds -> Just $ NSet    (map (fmap (fromMaybe nNull)) binds)
        NRecSet binds -> Just $ NRecSet (map (fmap (fromMaybe nNull)) binds)

        NLet binds (Just body@(Fix (Compose (Ann _ x)))) ->
            Just $ case mapMaybe pruneBinding binds of
                [] -> x
                xs -> NLet xs body

        NSelect (Just aset) attr alt ->
            Just $ NSelect aset (NE.map pruneKeyName attr) (join alt)

        -- These are the only short-circuiting binary operators
        NBinary NAnd (Just (Fix (Compose (Ann _ larg)))) _ -> Just larg
        NBinary NOr  (Just (Fix (Compose (Ann _ larg)))) _ -> Just larg

        -- If the function was never called, it means its argument was in a
        -- thunk that was forced elsewhere.
        NBinary NApp Nothing (Just _) -> Nothing

        -- The idea behind emitted a binary operator where one side may be
        -- invalid is that we're trying to emit what will reproduce whatever
        -- error the user encountered, which means providing all aspects of
        -- the evaluation path they ultimately followed.
        NBinary op Nothing (Just rarg) -> Just $ NBinary op nNull rarg
        NBinary op (Just larg) Nothing -> Just $ NBinary op larg nNull

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
        | otherwise = StaticKey "<unused?>" Nothing

    pruneParams :: Params (Maybe NExprLoc) -> Params NExprLoc
    pruneParams (Param n) = Param n
    pruneParams (ParamSet xs b n) =
        ParamSet (map (second (fmap (fromMaybe nNull))) xs) b n

    pruneBinding :: Binding (Maybe NExprLoc) -> Maybe (Binding NExprLoc)
    pruneBinding (NamedVar _ Nothing)  = Nothing
    pruneBinding (NamedVar xs (Just x)) =
        Just (NamedVar (NE.map pruneKeyName xs) x)
    pruneBinding (Inherit _ [])  = Nothing
    pruneBinding (Inherit m xs)  =
        Just (Inherit (join m) (map pruneKeyName xs))

nNull :: NExprLoc
nNull = Fix (Compose (Ann (SrcSpan nullPos nullPos) (NConstant NNull)))

nullAnn :: SrcSpan
nullAnn = SrcSpan nullPos nullPos

nullPos :: SourcePos
nullPos = SourcePos "<unknown>" (mkPos 0) (mkPos 0)

addTracing :: (MonadNix e m, MonadIO m,
              MonadReader Int n, Alternative n)
           => Alg NExprLocF (m a) -> Alg NExprLocF (n (m a))
addTracing k v = do
    depth <- ask
    guard (depth < 2000)
    local succ $ do
        v'@(Compose (Ann span x)) <- sequence v
        return $ do
            opts :: Options <- asks (view hasLens)
            let rendered =
                    if verbose opts >= Chatty
                    then show (void x)
                    else show (prettyNix (Fix (Fix (NSym "?") <$ x)))
                msg x = "eval: " ++ replicate depth ' ' ++ x
            loc <- renderLocation span (text (msg rendered ++ " ..."))
            liftIO $ putStr $ show loc
            res <- k v'
            liftIO $ putStrLn $ msg (rendered ++ " ...done")
            return res

reducingEvalExpr
    :: (Framed e m, Exception r, MonadCatch m, MonadIO m)
    => (NExprLocF (m a) -> m a)
    -> Maybe FilePath
    -> NExprLoc
    -> m (NExprLoc, Either r a)
reducingEvalExpr eval mpath expr = do
    expr'  <- flagExprLoc =<< liftIO (reduceExpr mpath expr)
    eres   <- catch (Right <$> cata (addEvalFlags eval) expr') (pure . Left)
    expr'' <- pruneTree expr'
    return (fromMaybe nNull expr'', eres)
  where
    addEvalFlags k (FlaggedF (b, x)) = liftIO (writeIORef b True) *> k x
