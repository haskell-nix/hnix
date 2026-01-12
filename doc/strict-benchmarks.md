# Strict vs Lazy Build Benchmarks

Benchmarks comparing HNix builds with and without the `Strict` language extension.

## Test Configuration

**Expression**: `(import /home/connorbaker/nixpkgs {}).stdenv.drvPath`

**Command**:
```bash
/usr/bin/env time -v ./result-strict/bin/hnix \
  --store-dir /nix/store \
  --store-mode overlay \
  --no-store-read-through \
  --eval --eval-stats \
  --expr '(import /home/connorbaker/nixpkgs {}).stdenv.drvPath'
```

## Performance Comparison

| Build | Stats | Wall Time | Memory (RSS) |
|-------|-------|-----------|--------------|
| **Strict** | No | 8.83s | 726 MB |
| **Lazy** | No | 8.84s | 868 MB |
| **Strict** | Yes | 20.94s | 756 MB |
| **Lazy** | Yes | 23.16s | 2,101 MB |

## Key Findings

### Without Stats (Production Use Case)
- **Time**: Virtually identical (~8.8s)
- **Memory**: Strict uses **16% less** (726 MB vs 868 MB)

### With Stats Enabled (Debugging/Profiling)
- **Time**: Strict is **10% faster** (20.94s vs 23.16s)
- **Memory**: Strict uses **64% less** (756 MB vs 2,101 MB - a **2.8x difference**)

## Detailed Stats Comparison

Both builds evaluate identical work:

| Metric | Strict | Lazy | Difference |
|--------|--------|------|------------|
| Builtin total time | 20.57s | 21.90s | Lazy 6% slower |
| Expression eval time | 20.73s | 22.02s | Lazy 6% slower |
| Scope lookup time | 1.06s | 1.50s | Lazy **42% slower** |
| Thunk compute time | 20.09s | 21.35s | Lazy 6% slower |
| Page faults (with stats) | 187,652 | 523,653 | Lazy **2.8x more** |

## Analysis

### Why We Use Strict

1. **Memory efficiency**: The lazy build accumulates thunks that aren't forced promptly, especially in infrastructure code like stats collection. This explains the 2.8x memory difference when stats are enabled.

2. **Scope lookup performance**: The 42% slower scope lookup time in lazy mode suggests lazy evaluation of scope chains creates measurable overhead.

3. **No runtime penalty**: Without stats, both builds complete in ~8.8s. Strict provides memory savings without sacrificing speed.

4. **Better debugging experience**: Stats collection interacts poorly with laziness, causing memory bloat. Strict makes profiling and debugging more practical.
