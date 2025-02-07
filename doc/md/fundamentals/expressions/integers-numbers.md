---
sidebar_position: 1
---

# Integers and numbers

This section focuses on integers `Int` and introduces **natural numbers `Nat`.

The `Int` type represents all integers, both positive and negative.

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

## Natural numbers

The `Nat` type represents natural numbers, which are all non-negative integers (i.e., `0` and positive numbers):

```motoko
let n: Nat = 42;
let zero: Nat = 0;
// let negative: Nat = -1; // Error: Cannot assign a negative value to Nat (underflow)
```

## Unbounded natural numbers

Like `Int`, the `Nat` type is unbounded by default, allowing extremely large values without overflow:

```motoko
let hugeNat: Nat = 1_000_000_000_000_000;
```

## Bounded natural numbers

Motoko also provides bounded natural number types:

- `Nat8`  (8-bit unsigned integer, range: 0 to 255)
- `Nat16` (16-bit unsigned integer, range: 0 to 65,535)
- `Nat32` (32-bit unsigned integer, range: 0 to 4,294,967,295)
- `Nat64` (64-bit unsigned integer, range: 0 to 18,446,744,073,709,551,615)

Bounded `Nat` types are ideal when working with binary protocols, embedded systems, or hardware where size constraints matter:

```motoko
let overflowNat8: Nat8 = 256; // Error: literal out of range Nat8
```

## Key differences between Int and Nat

| Feature               | `Int`                      | `Nat`                    |
|-----------------------|----------------------------|--------------------------|
| Values supported      | Positive & Negative        | Only Non-negative        |
| Default behavior      | Unbounded                  | Unbounded                |
| Bounded variants      | `Int8`, `Int16`, `Int32`...| `Nat8`, `Nat16`, `Nat32`...|
| Overflow possibility  | Yes (for bounded types)    | Yes (for bounded types)  |
