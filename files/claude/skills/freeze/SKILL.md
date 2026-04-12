---
name: freeze
description: Re-generate Cabal freeze files.
---

The current repo has Cabal freeze files. These may be named
`cabal.project.freeze` or `cabal.*.config`. They likely are for a specific
version of GHC.

Run `cabal update`. Then, for each freeze file:

- Use `ghcup` to install and configure the appropriate version of GHC
- Run `cabal freeze`
- Overwrite the old version with the newly-generated one
- Build and test with the new freeze file
