---
name: haddock
description: Reviews Haskell comments for Haddock markup opportunities. Use when reviewing or improving Haskell documentation comments.
---

Review the Haskell code for opportunities to add or improve Haddock markup. Apply the following guidelines:

## Documentation comment placement

- Use `-- |` before a declaration to document it
- Use `-- ^` after a field or argument to document it inline
- Module-level docs go in a `--|` block before the `module` keyword

## Markup to apply inside doc comments

- Link to identifiers with `'identifier'` or `'M.identifier'` (when `Module` is
  imported qualified as `M`)
- Link to modules with `"Full.Module.Name"`
- Wrap inline code in `@...@` (e.g. `@5@`, `@'Just' x@`)
- Use `> code here` lines for multi-line code blocks
- Use `>>> expr` followed by the expected output for REPL-style examples
- Use `prop> property` for property-based test assertions in docs
- Use `/text/` for emphasis and `__text__` for bold
- Use `[link text](URL)` for external hyperlinks

## Lists and structure

- Use `* item` or `- item` for bulleted lists
- Use `(1) item` or `1. item` for numbered lists
- Use `[@term@]: description` for definition lists

## Module export sections

- Use `-- *`, `-- **`, `-- ***` to group exports into labelled sections

## What to flag

- Exported functions, types, and classes that have no Haddock comment at all
- Plain `--` comments on exported items that should be `-- |` or `-- ^`
- Doc comments that mention identifiers without linking them with `'...'`
- Doc comments that show code samples without `@...@` or `>` markup
- Doc comments with examples that could use `>>>` REPL format
- Module headers missing Description, or lacking export section groupings
