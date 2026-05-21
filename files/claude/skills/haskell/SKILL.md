---
name: haskell
description: Guides for Haskell code. Use when writing Haskell code.
---

- Annotate `fromIntegral` with both `TypeApplications`
- Avoid defining functions by cases on their last argument, use `LambdaCase`
- Avoid list indexing (`!!`)
- Avoid partial functions (`head`, `undefined`) except in tests
- Instead of nesting `Either`, `Maybe`, and tuples, make a new `data` type
- `LambdaCase` now supports `\cases` for multiple arguments
- Never return tuples of arity higher than 2, just make a new `data` type
- Never use `unsafeCoerce` or `unsafePerformIO`
- Only use `Either` for error handling, otherwise make a new `data` type
- Use `Data.Text.IO` for I/O

