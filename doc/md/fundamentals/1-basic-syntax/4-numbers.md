---
sidebar_position: 4
---

# Numbers

## Integers

[`Int`](/docs/motoko/base/Int) represents all integers, both positive and negative (e.g., -2, -1, 0, 1, 2).

For scenarios requiring fixed-size integers, Motoko offers bounded variants with specific bit-widths (`Int8`, `Int16`, `Int32`, `Int64`). These types can overflow if their limits are exceeded, resulting in a runtime error.

```motoko no-repl
let a: Int = -42;
let b: Int = 0;
let c: Int = 12345;
```

### Unbounded integers

By default, [`Int`](/docs/motoko/base/Int) is unbounded, meaning it can grow as large (or as small) as needed without causing overflow:

```motoko no-repl
let bigNumber: Int = 999_999_999_999_999;
```

### Bounded integers

For scenarios requiring fixed-size integers, Motoko offers bounded integer types with specific bit-widths:

- `Int8`  (8-bit signed integer)
- `Int16` (16-bit signed integer)
- `Int32` (32-bit signed integer)
- `Int64` (64-bit signed integer)

Bounded integers can overflow if their limits are exceeded, resulting in a runtime error:

```motoko
let overflowInt: Int8 = 128; // Error: literal out of range Int8
```

### References

- [Int](/docs/motoko/base/Int)
- [Int8](/docs/motoko/base/Int8)
- [Int16](/docs/motoko/base/Int16)
- [Int32](/docs/motoko/base/Int32)
- [Int64](/docs/motoko/base/Int64)


-------

## Natural numbers

The [`Nat`](/docs/motoko/base/Nat) type represents natural numbers, which are all non-negative integers (i.e., `0` and positive numbers):

```motoko no-repl
let n: Nat = 42;
let zero: Nat = 0;
// let negative: Nat = -1; // Error: Cannot assign a negative value to Nat (underflow)
```

### Unbounded natural numbers

Like [`Int`](/docs/motoko/base/Int), the [`Nat`](/docs/motoko/base/Nat) type is unbounded by default, allowing extremely large values without overflow:

```motoko no-repl
let hugeNat: Nat = 1_000_000_000_000_000;
```

### Bounded natural numbers

Motoko also provides bounded natural number types:

- `Nat8`  (8-bit unsigned integer, range: 0 to 255)
- `Nat16` (16-bit unsigned integer, range: 0 to 65,535)
- `Nat32` (32-bit unsigned integer, range: 0 to 4,294,967,295)
- `Nat64` (64-bit unsigned integer, range: 0 to 18,446,744,073,709,551,615)

Bounded [`Nat`](/docs/motoko/base/Nat) types are ideal when working with binary protocols, embedded systems, or hardware where size constraints matter:

```motoko
let overflowNat8: Nat8 = 256; // Error: literal out of range Nat8
```


### References

- [Nat](/docs/motoko/base/Nat)
- [Nat8](/docs/motoko/base/Nat8)
- [Nat16](/docs/motoko/base/Nat16)
- [Nat32](/docs/motoko/base/Nat32)
- [Nat64](/docs/motoko/base/Nat64)


## Comparing `Int` and `Nat`

| Feature               | [`Int`](/docs/motoko/base/Int)                      | [`Nat`](/docs/motoko/base/Nat)                    |
|-----------------------|----------------------------|--------------------------|
| Values supported      | Positive & negative        | Only non-negative        |
| Default behavior      | Unbounded                  | Unbounded                |
| Bounded variants      | `Int8`, `Int16`, `Int32`...| `Nat8`, `Nat16`, `Nat32`...|
| Overflow possibility  | Yes (for bounded types)    | Yes (for bounded types)  |

--------

## Floats

Floating-point numbers in Motoko are represented using the [`Float`](/docs/motoko/base/Float) type, which corresponds to a 64-bit double-precision floating-point number.

```motoko no-repl
let pi: Float = 3.14159;
let exp: Float = 2.71828;
```

### References

- [Float](/docs/motoko/base/Float)

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />