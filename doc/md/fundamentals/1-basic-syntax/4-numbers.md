---
sidebar_position: 4
---

# Numbers

## Natural numbers

The [`Nat`](https://internetcomputer.org/docs/motoko/core/Nat) type represents natural numbers, which are all non-negative integers (i.e., `0` and positive numbers).

```motoko no-repl
let n : Nat = 42;
let zero : Nat = 0;

```

Defining a `Nat` with a negative value is a compile time error:

``` motoko
let negative : Nat = -1; // Error: Cannot assign a negative value to Nat
```

### Unbounded natural numbers

Like [`Int`](https://internetcomputer.org/docs/motoko/core/Int), the [`Nat`](https://internetcomputer.org/docs/motoko/core/Nat) type is unbounded by default, allowing extremely large values without overflow.

```motoko no-repl
let hugeNat : Nat = 1_000_000_000_000_000;
```

### Bounded natural numbers

Motoko also provides bounded natural number types.

- [`Nat8`](https://internetcomputer.org/docs/motoko/core/Nat8)  (8-bit unsigned integer, range: 0 to 255)
- [`Nat16`](https://internetcomputer.org/docs/motoko/core/Nat16) (16-bit unsigned integer, range: 0 to 65,535)
- [`Nat32`](https://internetcomputer.org/docs/motoko/core/Nat32) (32-bit unsigned integer, range: 0 to 4,294,967,295)
- [`Nat64`](https://internetcomputer.org/docs/motoko/core/Nat64) (64-bit unsigned integer, range: 0 to 18,446,744,073,709,551,615)

Bounded [`Nat`](https://internetcomputer.org/docs/motoko/core/Nat) types are ideal when working with binary protocols, embedded systems, or hardware where size constraints matter.

```motoko no-repl
let trappingNat8 : Nat8 = 255+1; // trap: arithmetic overflow
```

## Integers

[`Int`](https://internetcomputer.org/docs/motoko/core/Int) represents all integers, both positive and negative (e.g., -2, -1, 0, 1, 2).

For scenarios requiring fixed-size integers, Motoko offers bounded variants with specific bit-widths ([`Int8`](https://internetcomputer.org/docs/motoko/core/Int8), [`Int16`](https://internetcomputer.org/docs/motoko/core/Int16), [`Int32`](https://internetcomputer.org/docs/motoko/core/Int32), [`Int64`](https://internetcomputer.org/docs/motoko/core/Int64)). These types can overflow if their limits are exceeded, resulting in a [runtime error](https://internetcomputer.org/docs/motoko/fundamentals/error-handling).

```motoko no-repl
let a : Int = -42;
let b : Int = 0;
let c : Int = 12345;
```

### Unbounded integers

The  [`Int`](https://internetcomputer.org/docs/motoko/core/Int) is unbounded, meaning its values can grow as large (or as small) as needed without causing over- or underflow.

```motoko no-repl
let bigNumber : Int = 999_999_999_999_999;
```

### Bounded integers

- [`Int8`](https://internetcomputer.org/docs/motoko/core/Int8)  (8-bit signed integer)
- [`Int16`](https://internetcomputer.org/docs/motoko/core/Int16) (16-bit signed integer)
- [`Int32`](https://internetcomputer.org/docs/motoko/core/Int32) (32-bit signed integer)
- [`Int64`](https://internetcomputer.org/docs/motoko/core/Int64) (64-bit signed integer)

Arithmetic on bounded integers can overflow if their limits are exceeded, resulting in a [runtime error](https://internetcomputer.org/docs/motoko/fundamentals/error-handling).

```motoko no-repl
let trappingInt8 : Int8 = 127+1; // trap: arithmetic overflow
```

## Comparing `Int` and `Nat`

| Feature               | [`Int`](https://internetcomputer.org/docs/motoko/core/Int)                      | [`Nat`](https://internetcomputer.org/docs/motoko/core/Nat)                    |
|-----------------------|----------------------------|--------------------------|
| Values supported      | Positive & negative        | Only non-negative        |
| Default behavior      | Unbounded                  | Unbounded                |
| Bounded variants      | [`Int8`](https://internetcomputer.org/docs/motoko/core/Int8), [`Int16`](https://internetcomputer.org/docs/motoko/core/Int16), [`Int32`](https://internetcomputer.org/docs/motoko/core/Int32)...| [`Nat8`](https://internetcomputer.org/docs/motoko/core/Nat8), [`Nat16`](https://internetcomputer.org/docs/motoko/core/Nat16), [`Nat32`](https://internetcomputer.org/docs/motoko/core/Nat32)...|
| Overflow possibility  | Yes (for bounded types)    | Yes (for bounded types)  |

## Floats

Floating-point numbers in Motoko are represented using the [`Float`](https://internetcomputer.org/docs/motoko/core/Float) type, which corresponds to a 64-bit double-precision floating-point number in [IEEE 754](https://en.wikipedia.org/wiki/IEEE_754) representation.

:::note Limited precision
Floating point numbers have limited precision and operations may inherently result in numerical errors.
:::

```motoko no-repl
let pi : Float = 3.14159;
let exp : Float = 2.71828;
let goldenRatio = 1.61803;
```

## Resources

- [`Int`](https://internetcomputer.org/docs/motoko/core/Int)
- [`Int8`](https://internetcomputer.org/docs/motoko/core/Int8)
- [`Int16`](https://internetcomputer.org/docs/motoko/core/Int16)
- [`Int32`](https://internetcomputer.org/docs/motoko/core/Int32)
- [`Int64`](https://internetcomputer.org/docs/motoko/core/Int64)
- [`Nat`](https://internetcomputer.org/docs/motoko/core/Nat)
- [`Nat8`](https://internetcomputer.org/docs/motoko/core/Nat8)
- [`Nat16`](https://internetcomputer.org/docs/motoko/core/Nat16)
- [`Nat32`](https://internetcomputer.org/docs/motoko/core/Nat32)
- [`Nat64`](https://internetcomputer.org/docs/motoko/core/Nat64)
- [`Float`](https://internetcomputer.org/docs/motoko/core/Float)

