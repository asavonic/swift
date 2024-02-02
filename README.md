# Draft: Swift AutoDiff loop unroll optimization

The patch enables loop unrolling for AutoDiff pullback functions by using the
corresponding VJP function to determine loop trip count. Currently it has the
following limitations:

- Works on only small loops: loops with tripcount >= 32 are rejected.
- Loops with control flow are not supported.
- Generic functions increase Unroll and Inline cost significantly.
- Optimization relies on Unroll and Inline to give the best results.. If
  Pullback and VJP functions are rejected by these optimizations, block tracing
  context can be eliminated, but the resulting code is still sub-optimal.

## Statistics

Use `-Xllvm -stats` to enable compiler statistics (printed to stderr at the end
of compilation). All statistics are printed by default. Below are the ones we're
interested in (grep for AutoDiff).

### Loop unroll
We want to ensure that "considered" equals "known trip
count". Otherwise it means that we missed the unrolling. If this happens,
"missed" or "rejected" will be greater than zero.
```
         1 sil-loopunroll                   - Number of AutoDiff pullback functions considered for unrolling
         1 sil-loopunroll                   - Number of AutoDiff pullback with known trip count
         1 sil-loopunroll                   - Number of AutoDiff pullback functions that used the unroll bonus
         0 sil-loopunroll                   - Number of AutoDiff unrollable pullback functions missed by the analysis
         0 sil-loopunroll                   - Number of AutoDiff unrollable pullback functions rejected by the heuristic
```

### Inline
"Considered" should be equal to "inlined". Otherwise the heuristic may have
rejected functions as too large.
```
         2 sil-inliner                      - Number of AutoDiff functions considered for inlining
         2 sil-inliner                      - Number of AutoDiff functions inlined
         2 sil-inliner                      - Number of AutoDiff functions that used the inline bonus
```

### Post unroll
"Number of unrolled AutoDiff Pullback functions" should be equal to "interfaces
changed".
```
         1 sil-autodiff-post-unroll         - Number of unrolled AutoDiff Pullback/VJP missed
         1 sil-autodiff-post-unroll         - Number of unrolled AutoDiff Pullback/VJP interfaces changed
         1 sil-autodiff-post-unroll         - Number of unrolled AutoDiff Pullback functions
```

## Options

- `sil-unroll-autodiff-threshold`: loop unroll threshold for
  AutoDiffpullback functions.  Set to 0 to use the threshold for
  regular functions, N > 0 for a specific threshold. By default the
  threshold for pullbacks is 5 times the regular threshold.

- `sil-inline-autodiff-closure-benefit`: give inline benefit to AutoDiff
  derivative functions for each closure they capture in a pullback
  function. Default is 10, higher value will give AutoDiff VJPs with
  closures (and loops) more chance for inlining.

- `sil-autodiff-enable-unroll`: enable computation of trip count for AutoDiff
  pullback functions. Default is true.

- `sil-autodiff-enable-simplify`: enable simplification of AutoDiff pullback
  functions after loop unrolling. Default is true.

- `sil-autodiff-enable-post-unroll`: enable cleanup of block tracing
  context after unrolling. Default is true.

- `sil-autodiff-enable-late-opt`: enable additional late optmization
  passes. Helps to optimize further after AutoDiffPostUnroll. Default
  is true.

- `sil-autodiff-debug-verbose`: this option will enable more logging in
  AutoDiffBlockTracingAnalysis. Specifically, the compiler will dump
  SIL for all VJP and pullback and VJP functions. Default is false.

## Debug logging
The follwing option will enable logging for all relevant passes:
```
-Xllvm -debug-only=sil-loopunroll,sil-simplify-cfg,sil-inliner,sil-autodiff-post-unroll,sil-autodiff-bta
```