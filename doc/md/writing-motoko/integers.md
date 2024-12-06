---
sidebar_position: 11
---

# Integers and numbers




Motoko offers a variety types to represent integers and natural numbers, with the usual suite of arithmetic operators (`+`, `-`, `*`, '/' etc.) and comparison operators (`==`, `!=`,  `<`, `>`, `<=`, `>=`).

The types [`Int`](../base/Int.md) and [`Nat`](../base/Nat.md) are unbounded, meaning their values can grow to arbitrary size, limited only by memory.

The type [`Nat`](../base/Nat.md) is a subtype of [`Int`](../base/Int.md), so you can always supply a value of type [`Nat`](../base/Nat.md) where and [`Int`](../base/Int.md) is expected but not vice versa.

Motoko also provides bounded, or fixed-size integers and naturals, using a suffix to indicate the size of the type, in bits.

Thus [`Int8`](../base/Int8.md), [`Int16`](../base/Int16.md) and [`Int32`](../base/Int32.md) and [`Int64`](../base/Int64.md) are 8-, 16-, 32- and 64-bit integer types, while [`Nat8`](../base/Nat8.md), [`Nat16`](../base/Nat16.md), [`Nat32`](../base/Nat32.md), and [`Nat64`](../base/Nat64.md) are 8-, 16-, 32- and 64-bit natural types.

An arithmetic operation on a value of a fixed-size type will trap if its result exceeds the bounds of the fixed-size type, either due to overflow or underflow.
For example, `255 : Nat8 + 3` traps, because 258 is too large for a [`Nat8`](../base/Nat8.md).

Wrapping, non-trapping, versions of the usual arithmetic operations, performing modular arithmetic, are available by suffixing the usual operator with a `%`. For example, `255 : Nat8 +% 3` evaluates to `2`.

The type of an integer or natural constant is determined by the textual context
of the constant, defaulting to [`Int`](../base/Int.md) for negative constants and [`Nat`](../base/Nat.md) for positive ones.
Otherwise, the type of a constant can be indicated using a type annotation, for example `0 : Nat8`.

One can force a nonnegative constant to be interpreted as an [`Int`](../base/Int.md) using an explicit sign, for example  `+1` is the [`Int`](../base/Int.md) one.

For convenience, in-place updates of a variable or array element can be written by using a compound assignment operator, combining an arithmetic operation with the assignment operator `:=` . E.g. `x += 1` is short-hand for `x := x + 1` and combines addition `+` with assignment.

Motoko does not provide any implicit conversions between numeric types. Instead, base library functions like `Nat8.toNat`  and `Nat8.fromNat` should be used for explicit conversion.

To illustrate working with numbers, here is an example calculator program that creates a single actor with several public entry-point functions to perform basic arithmetic operations using integers.

## Using integers

```motoko file=../examples/Calc.mo
```

You might notice that this sample code uses integer ([`Int`](../base/Int.md)) data types, enabling you to use positive or negative numbers. If you wanted to restrict the functions in this calculator code to only use positive numbers, you could change the data type to only allow natural ([`Nat`](../base/Nat.md)) data.

This program supports the following function calls:

-   The `add` function call accepts input and performs addition.

-   The `sub` function call accepts input and performs subtraction.

-   The `mul` function call accepts input and performs multiplication.

-   The `div` function call accepts input and performs division. It also includes code to prevent the program from attempting to divide by zero.

-   The `clearall` function clears the `cell` value stored as the result of previous operations, resetting the `cell` value to zero.

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />