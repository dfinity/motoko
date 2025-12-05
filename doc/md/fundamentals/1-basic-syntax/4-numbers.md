---
sidebar_position: 4
---

# Numbers

## Natural numbers

The [`Nat`](../../core/Nat.md) type represents natural numbers, which are all non-negative integers (i.e., `0` and positive numbers).

```motoko no-repl
let n : Nat = 42;
let zero : Nat = 0;

```

Defining a `Nat` with a negative value is a compile time error:

``` motoko
let negative : Nat = -1; // Error: Cannot assign a negative value to Nat
```

### Unbounded natural numbers

Like [`Int`](../../core/Int.md), the [`Nat`](../../core/Nat.md) type is unbounded by default, allowing extremely large values without overflow.

```motoko no-repl
let hugeNat : Nat = 1_000_000_000_000_000;
```

### Bounded natural numbers

Motoko also provides bounded natural number types.

- [`Nat8`](../../core/Nat8.md)  (8-bit unsigned integer, range: 0 to 255)
- [`Nat16`](../../core/Nat16.md) (16-bit unsigned integer, range: 0 to 65,535)
- [`Nat32`](../../core/Nat32.md) (32-bit unsigned integer, range: 0 to 4,294,967,295)
- [`Nat64`](../../core/Nat64.md) (64-bit unsigned integer, range: 0 to 18,446,744,073,709,551,615)

Bounded [`Nat`](../../core/Nat.md) types are ideal when working with binary protocols, embedded systems, or hardware where size constraints matter.

```motoko no-repl
let trappingNat8 : Nat8 = 255+1; // trap: arithmetic overflow
```

## Integers

[`Int`](../../core/Int.md) represents all integers, both positive and negative (e.g., -2, -1, 0, 1, 2).

For scenarios requiring fixed-size integers, Motoko offers bounded variants with specific bit-widths ([`Int8`](../../core/Int8.md), [`Int16`](../../core/Int16.md), [`Int32`](../../core/Int32.md), [`Int64`](../../core/Int64.md)). These types can overflow if their limits are exceeded, resulting in a [runtime error](../9-error-handling.md).

```motoko no-repl
let a : Int = -42;
let b : Int = 0;
let c : Int = 12345;
```

### Unbounded integers

The  [`Int`](../../core/Int.md) is unbounded, meaning its values can grow as large (or as small) as needed without causing over- or underflow.

```motoko no-repl
let bigNumber : Int = 999_999_999_999_999;
```

### Bounded integers

- [`Int8`](../../core/Int8.md)  (8-bit signed integer)
- [`Int16`](../../core/Int16.md) (16-bit signed integer)
- [`Int32`](../../core/Int32.md) (32-bit signed integer)
- [`Int64`](../../core/Int64.md) (64-bit signed integer)

Arithmetic on bounded integers can overflow if their limits are exceeded, resulting in a [runtime error](../9-error-handling.md).

```motoko no-repl
let trappingInt8 : Int8 = 127+1; // trap: arithmetic overflow
```

## Comparing `Int` and `Nat`

| Feature               | [`Int`](../../core/Int.md)                      | [`Nat`](../../core/Nat.md)                    |
|-----------------------|----------------------------|--------------------------|
| Values supported      | Positive & negative        | Only non-negative        |
| Default behavior      | Unbounded                  | Unbounded                |
| Bounded variants      | [`Int8`](../../core/Int8.md), [`Int16`](../../core/Int16.md), [`Int32`](../../core/Int32.md)...| [`Nat8`](../../core/Nat8.md), [`Nat16`](../../core/Nat16.md), [`Nat32`](../../core/Nat32.md)...|
| Overflow possibility  | Yes (for bounded types)    | Yes (for bounded types)  |

## Floats

Floating-point numbers in Motoko are represented using the [`Float`](../../core/Float.md) type, which corresponds to a 64-bit double-precision floating-point number in [IEEE 754](https://en.wikipedia.org/wiki/IEEE_754) representation.

:::note Limited precision
Floating point numbers have limited precision and operations may inherently result in numerical errors.
:::

```motoko no-repl
let pi : Float = 3.14159;
let exp : Float = 2.71828;
let goldenRatio = 1.61803;
```

## Resources

- [`Int`](../../core/Int.md)
- [`Int8`](../../core/Int8.md)
- [`Int16`](../../core/Int16.md)
- [`Int32`](../../core/Int32.md)
- [`Int64`](../../core/Int64.md)
- [`Nat`](../../core/Nat.md)
- [`Nat8`](../../core/Nat8.md)
- [`Nat16`](../../core/Nat16.md)
- [`Nat32`](../../core/Nat32.md)
- [`Nat64`](../../core/Nat64.md)
- [`Float`](../../core/Float.md)

