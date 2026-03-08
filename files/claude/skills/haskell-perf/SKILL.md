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

Profile a specific test case or executable:

```bash
# Profile with RTS stats, perf counters, heap profile, cost centers
.agents/skills/perf/scripts/profile.py <executable> <args>

# Optional: include valgrind instruction counting (slow)
.agents/skills/perf/scripts/profile.py --with-valgrind <executable> <args>

# Output: profile-results-<timestamp>/ directory with:
#   - <binary>.prof (cost center report)
#   - <binary>.hp (heap profile with parsed statistics)
#   - perf-stats.log (CPU performance counters: instructions, cycles, IPC, cache misses, branch misses)
#   - Summary printed to stdout
```

### Test suite timing

Record timing for each test in the test suite:

```bash
# Run test suite and record per-test timings
.agents/skills/perf/scripts/test-timings.py <executable>

# Output:
#   - Prints summary to stdout (slowest tests, pass/fail counts)
#   - Saves detailed results to test-timings-<timestamp>.txt
#   - Handles duplicate test names correctly
```

Use this to establish baseline performance and identify slow tests.

## Workflow

For each improvement identified, make a TODO list with the following steps:

- If possible, identify an existing test or create a new test that decisively demonstrates the particular slowdown
- Before making the improvement, establish baseline metrics:

  - Run `.agents/skills/perf/scripts/test-timings.py` to capture per-test timings
  - Save the output file (test-timings-<timestamp>.txt) for comparison
  - On a particular test, run `.agents/skills/perf/scripts/profile.py` to get detailed metrics

- If the change is in a submodule, make a new `perf` branch on that submodule before making changes
- Make the change
- Re-run both measurement scripts and compare:

  - Compare detailed metrics
  - Check if target tests got faster
  - Compare total test suite time

- Note that wall clock timings are noisy, so focus more on the detailed `profile.py` output
- If the change resulted in the expected improvement, commit the change
- The commit message should include a brief overview of the improved stats

## Important notes

**CRUCIAL!**

You *must* collect before/after measurements using `profile.py` for each change.
Make only one change at a time.

Wall clock time is noisy, don't depend on it.
