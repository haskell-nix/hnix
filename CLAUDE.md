# HNix Codebase Guide for Claude Code

This guide provides essential context for working with HNix - a Haskell implementation of the Nix expression language that uses advanced functional programming techniques including recursion schemes and abstract definitional interpreters.

## Build and Development Commands

### Primary Development Workflow
```bash
# Enter development environment (recommended)
nix-shell

# Build the project
cabal v2-configure
cabal v2-build

# Run tests
cabal v2-test                          # Default test suite
env ALL_TESTS=yes cabal v2-test        # All tests including Nixpkgs parsing
env NIXPKGS_TESTS=yes cabal v2-test    # Only Nixpkgs tests

# REPL for interactive development
cabal v2-repl

# Run the executable
cabal v2-run hnix -- --help
cabal v2-run hnix -- --eval --expr '1 + 1'
cabal v2-run hnix -- --repl

# Benchmarks
cabal v2-bench
```

### Debugging and Profiling
```bash
# Full trace output with debug info
cabal v2-configure --enable-tests --enable-profiling --flags=profiling --flags=tracing
cabal v2-run hnix -- -v5 --trace <args> +RTS -xc

# Generate profiling data (upload .prof to speedscope.app)
cabal v2-run --enable-profiling --flags=profiling --enable-library-profiling \
  --profiling-detail='all-functions' hnix -- \
  --eval --expr '(import <nixpkgs> {}).firefox.outPath' +RTS -Pj

# Reduce test cases for debugging
hnix --reduce bug.nix --eval --expr 'import <nixpkgs> {}'
```

## Architecture: Recursion Schemes and Abstract Definitional Interpreters

HNix demonstrates production use of recursion schemes as described in John Wiegley's article "A win for recursion schemes". Understanding this architecture requires examining how multiple modules interact.

### Core Expression Architecture

The expression type uses a **functor-based representation** that separates structure from recursion:

```haskell
-- src/Nix/Expr/Types.hs
data NExprF r  -- Functor with 18 constructors (NConstant, NStr, NSym, NList, etc.)
type NExpr = Fix NExprF  -- Fixed point gives recursive structure

-- src/Nix/Expr/Types/Annotated.hs
type NExprLocF = AnnF SrcSpan NExprF  -- Composed annotation
type NExprLoc = Fix NExprLocF         -- Location-annotated expressions
```

This separation enables:
- **Parser** produces `NExprLoc` (with source positions)
- **Evaluator** works with `NExprF` (pure functor)
- **Pretty-printer** consumes `NExpr` (fixed point)
- **Annotations** compose via `Compose` without modifying core types

### Abstract Definitional Interpreters (ADI)

The `adi` function (`src/Nix/Utils.hs:345-351`) enables behavior injection at recursion boundaries:

```haskell
adi :: Functor f => Transform f a -> Alg f a -> Fix f -> a
```

Key applications:
- **Error context** (`src/Nix/Eval.hs`): `evalWithMetaInfo = adi addMetaInfo evalContent`
- **Frame tracking** during evaluation without modifying core logic
- **Scoping** and **variable resolution** layers

### Program Reduction for Debugging

`src/Nix/Reduce.hs` implements test case minimization using flagged trees:

```haskell
newtype FlaggedF f r = FlaggedF (IORef Bool, f r)  -- Wraps with evaluation markers
type Flagged f = Fix (FlaggedF f)

-- Algorithm: flag → evaluate → prune unreferenced → simplify logic
pruneTree :: MonadIO n => Options -> Flagged NExprLocF -> n (Maybe NExprLoc)
```

This reduces massive expressions (1.2M lines) to minimal reproducers (<10k lines).

### Value System with Free Monads

Values use Free monads for lazy evaluation (`src/Nix/Value.hs`):

```haskell
type NValue t f m = Free (NValue' t f m) t
-- Pure t = thunk (unevaluated)
-- Free (f (NValueF ...)) = evaluated value
```

Bidirectional pattern synonyms (v0.17.0) unify construction/deconstruction.

### Thunk Abstraction

Type class-based thunks (`src/Nix/Thunk`) support multiple evaluation strategies:

```haskell
class MonadThunk t m a | t -> m, t -> a where
  thunk :: m a -> m t    -- Create thunk
  force :: t -> m a      -- Force evaluation
```

## Key Module Organization

### Expression Layer (`src/Nix/Expr/*`)
- `Types.hs` - Core `NExprF` functor and `NExpr` type
- `Types/Annotated.hs` - Annotation composition via `Compose`
- `Strings.hs` - String interpolation/antiquotation
- `Shorthands.hs` - DSL for programmatic expression construction

### Evaluation Layer (`src/Nix/`)
- `Eval.hs` - Core evaluation with `MonadEval` type class
- `Exec.hs` - Execution context and higher-level evaluation
- `Reduce.hs` - Expression reduction and test case minimization
- `Utils.hs` - `adi` and other recursion schemes

### Value Layer (`src/Nix/Value/*`)
- `Value.hs` - Free monad-based value representation
- `Monad.hs` - Monadic value operations
- `Equal.hs` - Value equality semantics

### Thunk Layer (`src/Nix/Thunk/*`)
- Abstract thunk interface for lazy evaluation
- Support for both lazy and strict evaluation modes

### Built-ins (`src/Nix/Builtins.hs`)
- 100+ built-in functions
- Recent additions: `path`, `isPathNix`, `ceil`, `floor`, `hashFile`, `groupBy`

## Important Implementation Details

### Custom Prelude
Uses `relude` instead of standard Prelude (`NoImplicitPrelude` enabled globally).

### Position Tracking
Custom `NSourcePos` type (not Megaparsec's) for performance - addresses issues #1026, #746.

### String Context
Nix strings carry derivation dependencies - crucial for correct store path handling.

### Store Integration
**Warning**: `derivationStrict` produces real `/nix/store` entries via `hnix-store-remote`.

### Test Environment Variables
- `ALL_TESTS` - Run all tests including slow ones
- `NIXPKGS_TESTS` - Test Nixpkgs parsing
- `PRETTY_TESTS` - Pretty-printing round-trip tests

## Current Goals and Status

**Primary Goal**: Evaluate all of Nixpkgs (`hnix --eval --expr "import <nixpkgs> {}" --find`)

**Working**: Parser, lazy evaluation, most built-ins, REPL, type inference, store integration
**In Progress**: Full Nixpkgs evaluation, performance optimization

Tests are disabled by default (`doCheck = false` in `default.nix`) due to `hnix-store-remote` test harness needs.

## Resources

- [Design of the HNix code base](https://github.com/haskell-nix/hnix/wiki/Design-of-the-HNix-code-base)
- [Win for Recursion Schemes](https://newartisans.com/2018/04/win-for-recursion-schemes/) - Essential architectural context
- [Project Status Wiki](https://github.com/haskell-nix/hnix/wiki/Project-status)
- [Gitter Chat](https://gitter.im/haskell-nix/Lobby)