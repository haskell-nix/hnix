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
import           Data.Fix
import           Data.Functor.Compose
import           Data.IORef
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromMaybe, catMaybes, mapMaybe)
import           Data.Text (Text)
import           Nix.Atoms
import           Nix.Expr
import           Nix.Stack
import           Nix.Utils
import           Text.Megaparsec.Pos

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
           then Just (Fix (Compose (fmap prune x)))
           else Nothing
  where
    prune :: NExprF (Maybe NExprLoc) -> NExprF NExprLoc
    prune = \case
        NStr str                  -> NStr (pruneString str)
        NHasAttr (Just aset) attr -> NHasAttr aset (NE.map pruneKeyName attr)
        NList l                   -> NList (catMaybes l)
        NSet binds                -> NSet (mapMaybe pruneBinding binds)
        NRecSet binds             -> NRecSet (mapMaybe pruneBinding binds)
        NAbs params (Just body)   -> NAbs (pruneParams params) body

        NLet binds (Just body@(Fix (Compose (Ann _ x)))) ->
            case mapMaybe pruneBinding (NE.toList binds) of
                [] -> x
                b:bs -> NLet (b:|bs) body

        NSelect (Just aset) attr alt ->
            NSelect aset (NE.map pruneKeyName attr) (join alt)

        -- These are the only short-circuiting binary operators
        NBinary NAnd (Just (Fix (Compose (Ann _ larg)))) Nothing -> larg
        NBinary NOr  (Just (Fix (Compose (Ann _ larg)))) Nothing -> larg

        -- If the scope of a with was never referenced, it's not needed
        NWith Nothing (Just (Fix (Compose (Ann _ body)))) -> body

        NAssert Nothing _ ->
            error "How can an assert be used, but its condition not?"

        NAssert _ (Just (Fix (Compose (Ann _ body)))) -> body

        NIf Nothing _ _ ->
            error "How can an if be used, but its condition not?"

        NIf _ Nothing (Just (Fix (Compose (Ann _ f)))) -> f
        NIf _ (Just (Fix (Compose (Ann _ t)))) Nothing -> t

        x -> fromMaybe nNull <$> x

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

nNull :: Fix (Compose (Ann SrcSpan) NExprF)
nNull = Fix (Compose (Ann (SrcSpan nullPos nullPos) (NConstant NNull)))
  where
    nullPos = SourcePos "<unknown>" (mkPos 0) (mkPos 0)

tracingEvalExpr :: (Framed e m, MonadIO m,
                   MonadCatch n, MonadIO n, Alternative n)
                => (NExprF (m v) -> m v) -> NExprLoc -> n (m (NExprLoc, v))
tracingEvalExpr eval expr = do
    expr' <- flagExprLoc expr
    res <- flip catch handle $ flip runReaderT (0 :: Int) $
        adiM (pure <$> eval . annotated . getCompose . snd . flagged)
             psi expr'
    return $ do
        v <- res
        expr'' <- pruneTree expr'
        return (fromMaybe nNull expr'', v)
  where
    handle err = error $ "Error during evaluation: "
        ++ show (err :: SomeException)

    psi k v@(Fix (FlaggedF (b, _x))) = do
        depth <- ask
        guard (depth < 200)
        local succ $ do
            action <- k v
            return $ withExprContext (stripFlags v) $ do
                traceM $ "eval: " ++ replicate (depth * 2) ' '
                    ++ show (stripAnnotation (stripFlags v))
                liftIO $ writeIORef b True
                res <- action
                traceM $ "eval: " ++ replicate (depth * 2) ' ' ++ "."
                return res
