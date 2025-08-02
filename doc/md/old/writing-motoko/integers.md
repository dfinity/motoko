---
sidebar_position: 11
---

# Integers and numbers




Motoko offers a variety types to represent integers and natural numbers, with the usual suite of arithmetic operators (`+`, `-`, `*`, '/' etc.) and comparison operators (`==`, `!=`,  `<`, `>`, `<=`, `>=`).

The types [`Int`](../core/Int.md) and [`Nat`](../core/Nat.md) are unbounded, meaning their values can grow to arbitrary size, limited only by memory.

The type [`Nat`](../core/Nat.md) is a subtype of [`Int`](../core/Int.md), so you can always supply a value of type [`Nat`](../core/Nat.md) where and [`Int`](../core/Int.md) is expected but not vice versa.

Motoko also provides bounded, or fixed-size integers and naturals, using a suffix to indicate the size of the type, in bits.

Thus [`Int8`](../core/Int8.md), [`Int16`](../core/Int16.md) and [`Int32`](../core/Int32.md) and [`Int64`](../core/Int64.md) are 8-, 16-, 32- and 64-bit integer types, while [`Nat8`](../core/Nat8.md), [`Nat16`](../core/Nat16.md), [`Nat32`](../core/Nat32.md), and [`Nat64`](../core/Nat64.md) are 8-, 16-, 32- and 64-bit natural types.

An arithmetic operation on a value of a fixed-size type will trap if its result exceeds the bounds of the fixed-size type, either due to overflow or underflow.
For example, `255 : Nat8 + 3` traps, because 258 is too large for a [`Nat8`](../core/Nat8.md).

Wrapping, non-trapping, versions of the usual arithmetic operations, performing modular arithmetic, are available by suffixing the usual operator with a `%`. For example, `255 : Nat8 +% 3` evaluates to `2`.

The type of an integer or natural constant is determined by the textual context
of the constant, defaulting to [`Int`](../core/Int.md) for negative constants and [`Nat`](../core/Nat.md) for positive ones.
Otherwise, the type of a constant can be indicated using a type annotation, for example `0 : Nat8`.

One can force a nonnegative constant to be interpreted as an [`Int`](../core/Int.md) using an explicit sign, for example  `+1` is the [`Int`](../core/Int.md) one.

For convenience, in-place updates of a variable or array element can be written by using a compound assignment operator, combining an arithmetic operation with the assignment operator `:=` . E.g. `x += 1` is short-hand for `x := x + 1` and combines addition `+` with assignment.

Motoko does not provide any implicit conversions between numeric types. Instead, core package functions like `Nat8.toNat`  and `Nat8.fromNat` should be used for explicit conversion.

To illustrate working with numbers, here is an example calculator program that creates a single actor with several public entry-point functions to perform basic arithmetic operations using integers.

## Using integers

```motoko file=../../examples/Calc.mo
```

You might notice that this sample code uses integer ([`Int`](../core/Int.md)) data types, enabling you to use positive or negative numbers. If you wanted to restrict the functions in this calculator code to only use positive numbers, you could change the data type to only allow natural ([`Nat`](../core/Nat.md)) data.

This program supports the following function calls:

-   The `add` function call accepts input and performs addition.

-   The `sub` function call accepts input and performs subtraction.

-   The `mul` function call accepts input and performs multiplication.

-   The `div` function call accepts input and performs division. It also includes code to prevent the program from attempting to divide by zero.

-   The `clearall` function clears the `cell` value stored as the result of previous operations, resetting the `cell` value to zero.

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />