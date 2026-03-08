---
name: haskell-perf
description: Improve performance of Haskell code. Use when asked to profile or improve performance.
---

Purpose:

- Perform fine-grained timing and heap profiling
- Analyze hot spots and identify possible improvements

## Setup

Enable profiling in `cabal.project.local`:

```
profiling: True
profiling-detail: late
package *
  profiling: True
  ghc-options: -fprof-late
```

Then rebuild:

```bash
cabal build pkg:<executable>
```

This takes a long time (30+ minutes) for a fresh build with profiling. Run in background.

## Commands

### Detailed profiling

Profile a specific test case:

```bash
# Profile with RTS stats, perf counters, heap profile, cost centers
.agents/skills/perf/scripts/profile.py <executable> <args>

# Include valgrind instruction counting if perf is not working/available
.agents/skills/perf/scripts/profile.py --with-valgrind <executable> <args>

# Output: profile-results-<timestamp>/ directory with:
#   - <binary>.prof (cost center report)
#   - <binary>.hp (heap profile with parsed statistics)
#   - perf-stats.log (CPU performance counters: instructions, cycles, IPC, cache misses, branch misses)
#   - Summary printed to stdout
```

## Commentary

While quick fixes line pragmas (`INLINE`, `SPECIALIZE`) or strictness
annotations can be helpful, it is often most impactful to make changes to the
overall algorithm or structure of the computation. Instead of just looking
locally at the cost centers, think broadly about the context. Why is this
function a hot spot? What could be done to restructure the computation more
broadly? Don't be afraid to make big changes.

## Workflow

For each improvement identified, follow these steps:

- If possible, identify an existing test or create a new test that decisively demonstrates the particular slowdown
- Before making the improvement, establish baseline metrics using `profile.py`
- If the change is in a submodule, make a new `perf` branch on that submodule before making changes
- Make the change
- Re-run `profiile.py` and compare the metrics
- If the change resulted in the expected improvement, commit the change
- The commit message should the impact on instruction count and bytes allocated.

## Important notes

**CRUCIAL!**

You MUST collect before/after measurements using `profile.py` for each change.
Make only one change at a time.

Wall clock time is noisy, it's not even worth measuring. DO NOT USE wall-clock
time.
