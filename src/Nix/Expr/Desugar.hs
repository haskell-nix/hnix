-- | AST-level desugaring for nested attribute path bindings.
--
-- This module handles the transformation of attribute set bindings before evaluation.
-- It merges bindings that share a common prefix, such as:
--
-- @
-- { a.b = { x = 1; }; a.b.z = 2; }
-- @
--
-- which should become:
--
-- @
-- { a = { b = { x = 1; z = 2; }; }; }
-- @
--
-- This is necessary because HNix's evaluation-time desugaring cannot inspect
-- whether a value is an attribute set to merge it with path bindings.
module Nix.Expr.Desugar
  ( desugarExprLoc
  ) where

import           Nix.Prelude
import           Data.Fix                       ( Fix(..) )
import qualified Data.HashMap.Strict           as HM
import qualified Data.List.NonEmpty            as NE
import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated

-- | Desugar an annotated Nix expression (NExprLoc).
-- Transforms the entire AST bottom-up, merging attribute set bindings.
{-# INLINABLE desugarExprLoc #-}
desugarExprLoc :: NExprLoc -> NExprLoc
desugarExprLoc = transform desugarNode
  where
    -- Bottom-up transformation for annotated expressions
    transform :: (NExprLoc -> NExprLoc) -> NExprLoc -> NExprLoc
    transform f (Fix (AnnF ann expr)) =
      f $ Fix $ AnnF ann $ fmap (transform f) expr

    desugarNode :: NExprLoc -> NExprLoc
    desugarNode (Fix (AnnF ann (NSet rec binds))) =
      Fix $ AnnF ann $ NSet rec $ desugarBindings binds
    desugarNode (Fix (AnnF ann (NLet binds body))) =
      Fix $ AnnF ann $ NLet (desugarBindings binds) body
    desugarNode e = e

-- | Data type to track bindings grouped by their first static key.
data BindingGroup = BindingGroup
  { bgSingleKey :: Maybe (NExprLoc, NSourcePos)
    -- ^ The value and position if there's a single-key binding (a = expr)
  , bgPathBinds :: [(NAttrPath NExprLoc, NExprLoc, NSourcePos)]
    -- ^ Path bindings with the *remaining path* after first key.
    -- For a.b.c = expr with first key 'a', this stores (b :| [c], expr, pos).
    -- Uses list because we only prepend during construction (O(1)) and iterate during emission.
  }

emptyGroup :: BindingGroup
emptyGroup = BindingGroup Nothing []

-- | Desugar a list of bindings by merging those with shared prefixes.
--
-- The algorithm:
-- 1. Group all NamedVar bindings by their first static key
-- 2. For each group:
--    a. If there's only path bindings (no single-key), create nested set structure
--    b. If there's a single-key binding whose value is NSet, merge path bindings into it
--    c. If there's a single-key binding with a non-NSet value and path bindings exist,
--       this is a duplicate attribute error (emit both, let eval catch it)
--    d. If there's only a single-key binding, emit it unchanged
-- 3. Pass through Inherit bindings and dynamic-key bindings unchanged
desugarBindings :: [Binding NExprLoc] -> [Binding NExprLoc]
desugarBindings binds =
  let
    -- Partition into single-key, path, and other bindings
    (singleKeyBinds, pathBinds, otherBinds) = partitionBindings binds
  in
    -- Early return: if there are no path bindings (2+ component paths),
    -- there's nothing to desugar, return bindings unchanged to preserve order
    case pathBinds of
      [] -> binds
      _ ->
        let
          -- Group bindings by first key
          grouped :: HashMap VarName BindingGroup
          grouped = foldr insertPathBinding (foldr insertSingleKey HM.empty singleKeyBinds) pathBinds

          -- Emit merged bindings
          emitted = HM.toList grouped >>= uncurry emitGroup
        in
          -- Combine: other bindings (inherit, dynamic) + emitted static bindings
          otherBinds <> emitted

  where
    -- Partition bindings into static-first-key NamedVar and others
    -- Returns: (single-key bindings, path bindings, other bindings)
    partitionBindings :: [Binding NExprLoc]
                      -> ([(VarName, NExprLoc, NSourcePos)],           -- Single-key: (key, val, pos)
                          [(VarName, NAttrPath NExprLoc, NExprLoc, NSourcePos)],  -- Path: (firstKey, restPath, val, pos)
                          [Binding NExprLoc])                          -- Other bindings
    partitionBindings = foldr go ([], [], [])
      where
        go b@(Inherit _ _ _) (singles, paths, others) = (singles, paths, b : others)
        go b@(NamedVar (DynamicKey _ :| _) _ _) (singles, paths, others) = (singles, paths, b : others)
        go (NamedVar (StaticKey k :| ks) val pos) (singles, paths, others) =
          case ks of
            -- Single-key binding: a = expr
            [] -> ((k, val, pos) : singles, paths, others)
            -- Path binding: a.b.c = expr -> store (a, b :| [c], expr, pos)
            (k' : ks') -> (singles, (k, k' :| ks', val, pos) : paths, others)

    -- Insert a single-key binding into the grouped map
    insertSingleKey :: (VarName, NExprLoc, NSourcePos)
                    -> HashMap VarName BindingGroup
                    -> HashMap VarName BindingGroup
    insertSingleKey (key, val, pos) =
      HM.alter (Just . addSingle . fromMaybe emptyGroup) key
      where
        addSingle grp = grp { bgSingleKey = Just (val, pos) }

    -- Insert a path binding into the grouped map
    insertPathBinding :: (VarName, NAttrPath NExprLoc, NExprLoc, NSourcePos)
                      -> HashMap VarName BindingGroup
                      -> HashMap VarName BindingGroup
    insertPathBinding (key, restPath, val, pos) =
      HM.alter (Just . addPath . fromMaybe emptyGroup) key
      where
        addPath grp = grp { bgPathBinds = (restPath, val, pos) : bgPathBinds grp }

    -- Emit bindings for a group
    emitGroup :: VarName -> BindingGroup -> [Binding NExprLoc]
    emitGroup key grp =
      case (bgSingleKey grp, bgPathBinds grp) of
        -- Only single-key binding, no paths - emit as-is
        (Just (val, pos), []) ->
          [NamedVar (StaticKey key :| []) val pos]

        -- Only path bindings - create nested set structure
        (Nothing, paths@(_:_)) ->
          let nestedBinds = pathsToBindings paths
              pos = getFirstPos paths
              srcSpan = SrcSpan pos pos
              innerDesugared = desugarBindings nestedBinds
          in [NamedVar (StaticKey key :| []) (Fix (AnnF srcSpan (NSet NonRecursive innerDesugared))) pos]

        -- Both single-key and path bindings - need to merge
        (Just (val, pos), paths@(_:_)) ->
          case extractSetBindings val of
            -- Value is an attribute set - merge the bindings
            Just (srcSpan, rec, innerBinds) ->
              let pathBinds = pathsToBindings paths
                  merged = desugarBindings (innerBinds <> pathBinds)
              in [NamedVar (StaticKey key :| []) (Fix (AnnF srcSpan (NSet rec merged))) pos]
            -- Value is not an attribute set - cannot merge
            -- Emit both; the evaluator will report duplicate attribute error
            Nothing ->
              let singleBind = NamedVar (StaticKey key :| []) val pos
                  pathBindsExpanded = expandPathBindings key paths
              in singleBind : pathBindsExpanded

        -- Neither - shouldn't happen but handle gracefully
        (Nothing, []) -> []

    -- Get the position of the first path binding (for creating synthetic spans)
    getFirstPos :: [(NAttrPath NExprLoc, NExprLoc, NSourcePos)] -> NSourcePos
    getFirstPos [] = nullPos
    getFirstPos ((_, _, pos) : _) = pos

    -- Convert path bindings to regular bindings for the nested set
    pathsToBindings :: [(NAttrPath NExprLoc, NExprLoc, NSourcePos)] -> [Binding NExprLoc]
    pathsToBindings = fmap (\(path, val, pos) -> NamedVar path val pos)

    -- Expand path bindings back to full paths with the key prefix
    -- Used when we can't merge (non-attrset value)
    expandPathBindings :: VarName -> [(NAttrPath NExprLoc, NExprLoc, NSourcePos)] -> [Binding NExprLoc]
    expandPathBindings key = fmap (\(path, val, pos) ->
      NamedVar (StaticKey key `NE.cons` path) val pos)

    -- Extract bindings from an NSet expression
    extractSetBindings :: NExprLoc -> Maybe (SrcSpan, Recursivity, [Binding NExprLoc])
    extractSetBindings (Fix (AnnF s (NSet r bs))) = Just (s, r, bs)
    extractSetBindings _ = Nothing
