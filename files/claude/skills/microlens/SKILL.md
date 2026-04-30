---
name: microlens
description: Replace lens with microlens-* in Haskell packages
---

Replace `lens` with the appropriate `microlens-*` packages. Work one package at
a time, committing each in dependency order (dependencies before dependents). No
co-author metadata in commit messages.

## Package mapping

| Module | microlens package |
|--------|------------------|
| `Lens.Micro` | `microlens` |
| `Lens.Micro.Extras` | `microlens` |
| `Lens.Micro.FieldN` | `microlens` |
| `Lens.Micro.GHC` | `microlens-ghc` |
| `Lens.Micro.Mtl` | `microlens-mtl` |
| `Lens.Micro.TH` | `microlens-th` |

## Symbol routing

### `Lens.Micro`
`(^.)`, `(^?)`, `(.~)`, `(%~)`, `(^..)`, `lens`, `to`, `_Just`, `_Left`,
`_Right`, `each`, `Lens'`, `Traversal'`, `Traversal`, `Getting`, `ASetter`,
`at`, `ix`, and `(&)` **when imported alongside other `Lens.Micro` symbols**.

### `Lens.Micro.FieldN` (package: `microlens`)
`_1`, `_2`, `_3`, etc. — tuple field lenses. These are **not** re-exported
by `Lens.Micro` and must be imported from `Lens.Micro.FieldN` directly.

### `Lens.Micro.GHC`
No new symbols — add this import (and `microlens-ghc` to `build-depends`)
when using `at` or `ix` with `Map`, `IntMap`, `Set`, `Seq`, or
`ByteString`, to bring the required instances into scope. Do **not** use
for `Vector`.

### `Lens.Micro.Mtl`
`use`, `(.=)`, `(%=)`, `(+=)`, `zoom`, and `view` **when used
monadically** (i.e. in a `MonadReader` or state-transformer context).

### `Lens.Micro.Extras`
`view` **when used purely** (non-monadic), `preview`.

### `Lens.Micro.TH`
`makeLenses`.

### `Data.Function`
`(&)` **when it is the only symbol being imported** — never put a
lone `(&)` import into `Lens.Micro`.

## Import rules

- **Explicit import lists are required.** Never write `import Lens.Micro`
  without a symbol list.
- **Follow existing import conventions.**
  - If `Control.Lens` was imported qualified, do the same for `Lens.Micro.*`.
  - Put the `Lens.Micro.*` import in the same stanza that `Control.Lens` was in.
  - If the imports were sorted alphabetically, ensure they remain so.
- **Do not replace `(^.)` with `view` or vice-versa.** Leave operator usage
  as-is.
- **Minimize diff.** Do not reorganize imports beyond what the migration
  requires.

## Cabal / package metadata

- Remove `lens` from `build-depends` in every `.cabal` file.
- Add whichever `microlens-*` packages are actually used.
- If the package previously depended on `lens` for transitive
  re-exports, verify no symbols are now missing after the swap.

## Workflow

1. Identify all packages that depend on `lens`, sorted so dependencies
   come first.
2. For each package:
   a. Update `.cabal` `build-depends`.
   b. Update every Haskell source file's imports following the rules
      above.
   c. Build and fix any remaining type errors.
   d. Commit with a concise message (no co-author line).
