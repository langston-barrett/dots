---
name: hedgehog
description: >
  Guidelines for property-based tests for Haskell using Hedgehog. Use when
  writing or reviewing property-based tests in Haskell.
---

- Rather than using conditionals (`if`, `unless`, `when`) when preconditions
  aren't met, use `==>` or `discard` from Hedgehog.
- Ensure that generators have high coverage, consider writing tests that
  guarantee that the generators can generate all relevant shapes of data within
  some relatively small number of samples.
- Don't provide explanatory comments for very simple algebraic properties such
  as reflexivity of `==`.

Ensure that every nontrivial typeclass instance has tests for *each* of the
following properties:

- `Eq`
  - `x == x`
  - `x == y ==> y == x`
  - `x == y && y == z ==> x == z`
- `Ord`
  - `x <= y && y <= x ==> x == y`
  - `x <= y && y <= z ==> x <= z`
  - `x <= y || y <= x`
  - `compare x y == EQ ==> x == y`
- `Eq1`
  - `liftEq (==) x y == (x == y)`
- `Ord1`
  - `liftCompare compare x y == compare x y`
- `Hashable`
  - `x == y ==> hash x == hash y`

- `Functor`
  - `fmap id == id`
  - `fmap (f . g) == fmap f . fmap g`
- `Applicative`
  - `pure id <*> v == v`
  - `pure (.) <*> u <*> v <*> w == u <*> (v <*> w)`
  - `pure f <*> pure x == pure (f x)`
  - `u <*> pure y == pure ($ y) <*> u`
- `Monad`
  - `return x >>= f == f x`
  - `m >>= return == m`
  - `(m >>= f) >>= g == m >>= (\x -> f x >>= g)`

- `Traversable`
  - `traverse Identity == Identity`
  - `traverse (Compose . fmap g . f) == Compose . fmap (traverse g) . traverse f`

- `Bifunctor`
  - `bimap id id == id`
  - `bimap (f . g) (h . i) == bimap f h . bimap g i`
- `Bitraversable`
  - `bitraverse (Identity . f) (Identity . g) == Identity . bimap f g`
  - `bitraverse (Compose . fmap g . f) (Compose . fmap i . h) == Compose . fmap (bitraverse g i) . bitraverse f h`

- `Semigroup`
  - `(x <> y) <> z == x <> (y <> z)`
- `Monoid`
  - `x <> mempty == x`
  - `mempty <> x == x`
  - `(x <> y) <> z == x <> (y <> z)`

- `Alternative`
  - `empty <|> x == x`
  - `x <|> empty == x`
  - `(x <|> y) <|> z == x <|> (y <|> z)`
- `MonadPlus`
  - `mzero >>= f == mzero`
  - `m >> mzero == mzero`

## Property quick reference

<!-- From https://github.com/trailofbits/skills/tree/52314112b97ff0cf0364ae9875ad6f69023b5e2d/plugins/property-based-testing -->

| Property | Formula | When to Use |
|----------|---------|-------------|
| **Roundtrip** | `decode(encode(x)) == x` | Serialization, conversion pairs |
| **Idempotence** | `f(f(x)) == f(x)` | Normalization, formatting, sorting |
| **Invariant** | Property holds before/after | Any transformation |
| **Commutativity** | `f(a, b) == f(b, a)` | Binary/set operations |
| **Associativity** | `f(f(a,b), c) == f(a, f(b,c))` | Combining operations |
| **Identity** | `f(x, identity) == x` | Operations with neutral element |
| **Inverse** | `f(g(x)) == x` | encrypt/decrypt, compress/decompress |
| **Oracle** | `new_impl(x) == reference(x)` | Optimization, refactoring |
| **Easy to Verify** | `is_sorted(sort(x))` | Complex algorithms |
| **No Exception** | No crash on valid input | Baseline property |

**Strength hierarchy** (weakest to strongest):
No Exception → Type Preservation → Invariant → Idempotence → Roundtrip
