---
sidebar_position: 1
---

# Integers and numbers

This section focuses on integers `Int` and introduces natural numbers `Nat`.

Motoko supports both unbounded and bounded [integers](../basic-syntax/integers.md)

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
| Values supported      | Positive & negative        | Only non-negative        |
| Default behavior      | Unbounded                  | Unbounded                |
| Bounded variants      | `Int8`, `Int16`, `Int32`...| `Nat8`, `Nat16`, `Nat32`...|
| Overflow possibility  | Yes (for bounded types)    | Yes (for bounded types)  |
