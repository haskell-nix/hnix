# Attribute Set Desugaring Issue in HNix

This document describes a discovered bug in HNix's handling of mixed attribute path bindings and its implications for evaluating NixOS configurations.

## Problem Summary

HNix fails to correctly evaluate attribute sets that mix single-key bindings with nested path bindings sharing the same prefix:

```nix
# This fails in HNix but works in Nix:
{ a.b = { x = 1; y = 2; }; a.b.z = 3; }

# Expected: { a = { b = { x = 1; y = 2; z = 3; }; }; }
# HNix produces: { a = { b = { x = 1; y = 2; }; }; }  (z is lost)
```

This pattern appears frequently in NixOS modules, particularly in `alsa.nix`:

```nix
{
  options.hardware.alsa = {
    enable = lib.mkOption { ... };
    cardAliases = lib.mkOption { ... };
  };
  # Separate declaration at same path - triggers the bug
  options.hardware.alsa.enablePersistence = lib.mkOption { ... };
}
```

## Root Cause Analysis

### How Desugaring Works

The `desugarBinds` function in `src/Nix/Eval.hs` transforms nested attribute paths:

```nix
# Input bindings:
{ foo.a = 1; foo.b = 2; }

# After desugarBinds:
{ foo = { a = 1; b = 2; }; }
```

### The Bug

`desugarBinds` only collects bindings with **multi-component paths** (length >= 2). Single-key bindings pass through unchanged:

```haskell
collect (NamedVar (StaticKey x :| y : ys) val pos) = ...  -- Collected
collect x = pure $ pure x  -- Single-key: passes through unchanged
```

When we have:
- `b = { x = 1; }` - single-key binding (passes through)
- `b.z = 2` - path binding (collected under "b")

These become **two separate bindings** for key "b":
1. `b = { x = 1; }` (from passthrough)
2. `b = { z = 2; }` (from desugaring)

In `evalBinds`, the second binding **overwrites** the first, losing `x = 1`.

### Additional Bug: Duplicate Emission

The original `desugarBinds` uses `traverse (findBinding <=< collect)` which processes bindings sequentially. When two path bindings share the same first key:

```nix
{ foo.a = 1; foo.b = 2; }
```

The first binding is emitted **before** the second is collected:
1. Process `foo.a`: collect -> emit `foo = { a = 1; }`
2. Process `foo.b`: collect -> emit `foo = { a = 1; b = 2; }`

This produces **two bindings** for "foo", where the second overwrites the first. This was partially fixed by implementing two-phase processing.

## Attempted Fixes

### Fix 1: Two-Phase `desugarBinds`

Changed `desugarBinds` to process in two phases:
1. **Collect phase**: Process ALL bindings first to build complete state
2. **Emit phase**: Output one binding per unique key

```haskell
desugarBinds embed binds =
  let
    (collected, finalState) = runState (traverse collect binds) mempty
  in evalState (catMaybes <$> traverse (emitBinding finalState) collected) mempty
```

**Result**: Fixes duplicate emission for path-only bindings, but doesn't help with mixed single-key + path bindings.

### Fix 2: Merge in `attrSetAlter`

Added logic to `attrSetAlter` to detect when inserting at an existing single key, and merge if both values are attrsets:

```haskell
mergeOrInsert existing = do
  existingVal <- existing
  newVal <- val
  existingMay <- fromValueMay @(AttrSet v, PositionSet) existingVal
  newMay <- fromValueMay @(AttrSet v, PositionSet) newVal
  case (existingMay, newMay) of
    (Just (existingAttrs, _), Just (newAttrs, _)) -> merge...
    _ -> overwrite...
```

**Result**: Works for simple cases, but causes **infinite recursion** in complex NixOS configurations because forcing values during binding processing breaks lazy evaluation.

### Fix 3: Deferred Merge

Created a deferred merge thunk that delays forcing until access time:

```haskell
Just existing -> pure $ insertVal (deferredMerge existing val)
```

**Result**: Still causes infinite recursion. The fundamental issue is that mutual recursion in module configurations creates cycles that can't be broken by deferral alone.

## Why This Is Hard to Fix

1. **`desugarBinds` operates on `m v` (evaluation actions)**, not AST nodes. It cannot inspect whether a value is an `NSet` to extract its bindings.

2. **Forcing values during binding processing breaks `loebM`**. The recursive binding mechanism relies on lazy evaluation - values must not be forced until the fixed-point knot is tied.

3. **The module system creates complex circular dependencies**. Options reference config, config references options, and lazy evaluation is essential.

## Proper Fix Direction

The correct fix likely requires **AST-level desugaring** before evaluation:

1. Create a transformation `NExprLoc -> NExprLoc` that runs before `adi` evaluation
2. This transformation can inspect `NSet` expressions and merge bindings at the AST level
3. When encountering `b = { x = 1; }` and `b.z = 2`, extract the `NSet` bindings and merge

```haskell
-- Pseudocode for AST-level fix
desugarAst :: NExprLoc -> NExprLoc
desugarAst = transform $ \case
  NSet r binds -> NSet r (desugarBindsAst binds)
  NLet binds body -> NLet (desugarBindsAst binds) body
  e -> e

desugarBindsAst :: [Binding NExprLoc] -> [Binding NExprLoc]
-- Can inspect values: if binding is `key = NSet bindings`,
-- extract and merge with any `key.x = ...` bindings
```

This approach:
- Works at the AST level where structure is visible
- Runs before evaluation, so doesn't interfere with lazy evaluation
- Correctly handles all cases including single-key + path binding combinations

## Minimal Reproducers

### Reproducer 1: Single-key + path binding (simplest case)

```bash
# HNix - FAILS (z is missing)
nix develop ".?submodules=1#" --command cabal run hnix -- \
  --eval --strict --expr '{ a.b = { x = 1; }; a.b.z = 2; }'
# Output: { a = { b = { x = 1; }; }; }

# Standard Nix - WORKS
nix eval --impure --expr '{ a.b = { x = 1; }; a.b.z = 2; }'
# Output: { a = { b = { x = 1; z = 2; }; }; }
```

### Reproducer 2: Module system option declarations

```bash
# HNix - FAILS with "option does not exist"
nix develop ".?submodules=1#" --command cabal run hnix -- --eval --strict --expr '
let
  lib = import /home/connorbaker/nixpkgs/lib;
  result = lib.evalModules {
    modules = [{
      options.foo = {
        a = lib.mkOption { type = lib.types.str; default = ""; };
        b = lib.mkOption { type = lib.types.str; default = ""; };
      };
      # This separate declaration triggers the bug
      options.foo.c = lib.mkOption { type = lib.types.str; default = ""; };
      config.foo.b = "test";
    }];
  };
in result.config.foo.b
'

# Standard Nix - WORKS
nix eval --impure --expr '
let
  lib = import /home/connorbaker/nixpkgs/lib;
  result = lib.evalModules {
    modules = [{
      options.foo = {
        a = lib.mkOption { type = lib.types.str; default = ""; };
        b = lib.mkOption { type = lib.types.str; default = ""; };
      };
      options.foo.c = lib.mkOption { type = lib.types.str; default = ""; };
      config.foo.b = "test";
    }];
  };
in result.config.foo.b
'
# Output: "test"
```

### Reproducer 3: Real-world NixOS pattern (alsa.nix)

```bash
# This is the actual pattern from nixos/modules/services/audio/alsa.nix
# that triggers the bug during NixOS closure evaluation

nix develop ".?submodules=1#" --command cabal run hnix -- --eval --strict --expr '
let
  lib = import /home/connorbaker/nixpkgs/lib;
  result = lib.evalModules {
    modules = [{
      options.hardware.alsa = {
        enable = lib.mkOption { type = lib.types.bool; default = false; };
        cardAliases = lib.mkOption {
          type = lib.types.attrsOf (lib.types.submodule {
            options.driver = lib.mkOption { type = lib.types.str; };
            options.id = lib.mkOption { type = lib.types.int; default = 0; };
          });
          default = {};
        };
      };
      # This separate option declaration at the same path triggers the bug
      options.hardware.alsa.enablePersistence = lib.mkOption {
        type = lib.types.bool;
        default = false;
      };
      config.hardware.alsa.cardAliases.loopback.driver = "test";
    }];
  };
in result.config.hardware.alsa.cardAliases.loopback.driver
'
# HNix error: The option `hardware.alsa.cardAliases` does not exist
# Standard Nix: "test"
```

## Test Cases Summary

| Test | HNix Result | Expected |
|------|-------------|----------|
| `{ a.b = 1; a.c = 2; }` | `{ a = { b = 1; c = 2; }; }` | PASS |
| `let x.a = y; x.b = 1; y = 2; in x.a` | `2` | PASS (after NLet fix) |
| `{ a.b = { x = 1; }; a.b.z = 2; }` | `{ a = { b = { x = 1; }; }; }` | FAIL (z lost) |
| Module with separate option decl | Error: option doesn't exist | FAIL |

## Files Involved

- `src/Nix/Eval.hs`: `desugarBinds`, `attrSetAlter`, `evalBinds`
- `src/Nix/Exec.hs`: `evalExprLoc` entry point
- `src/Nix/Utils.hs`: `adi` recursion scheme

## References

- Original error: `The option 'hardware.alsa.cardAliases' does not exist`
- Location: `/home/connorbaker/nixpkgs/nixos/modules/services/audio/alsa.nix`
- Related: `mkSdConfigModule` undefined variable bug (fixed separately)
