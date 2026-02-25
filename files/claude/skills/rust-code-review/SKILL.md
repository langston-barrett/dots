---
name: rust-code-review
description: Guides for reviewing Rust code. Use when reviewing Rust code.
---

- Are there places where an API uses a callback, but doesn't need to?
- Are there places where an owned type and `.clone()` are used, but references would do?
- Is there anywhere that `{HashMap,Vec}::with_capacity` could be used instead of `new`?
