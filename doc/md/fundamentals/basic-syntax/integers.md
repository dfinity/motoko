---
sidebar_position: 4
---

# Integers

`Int` represents all integers, both positive and negative (e.g., -2, -1, 0, 1, 2).

For scenarios requiring fixed-size integers, Motoko offers bounded variants with specific bit-widths (`Int8`, `Int16`, `Int32`, `Int64`). These types can overflow if their limits are exceeded, resulting in a runtime error.

```motoko
let a: Int = -42;
let b: Int = 0;
let c: Int = 12345;
```

## Unbounded integers

By default, `Int` is unbounded, meaning it can grow as large (or as small) as needed without causing overflow:

```motoko
let bigNumber: Int = 999_999_999_999_999;
```

## Bounded integers

For scenarios requiring fixed-size integers, Motoko offers bounded integer types with specific bit-widths:

- `Int8`  (8-bit signed integer)
- `Int16` (16-bit signed integer)
- `Int32` (32-bit signed integer)
- `Int64` (64-bit signed integer)

Bounded integers can overflow if their limits are exceeded, resulting in a runtime error:

```motoko
// let overflowInt: Int8 = 128; // Error: literal out of range Int8
```

## References

- [Int](https://internetcomputer.org/docs/current/motoko/main/base/Int)
- [Int8](https://internetcomputer.org/docs/current/motoko/main/base/Int8)
- [Int16](https://internetcomputer.org/docs/current/motoko/main/base/Int16)
- [Int32](https://internetcomputer.org/docs/current/motoko/main/base/Int32)
- [Int64](https://internetcomputer.org/docs/current/motoko/main/base/Int64)
