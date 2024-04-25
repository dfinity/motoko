---
sidebar_position: 10
---

# Integers and numbers 


## Overview

Motoko offers a variety types to represent integers and natural numbers, with the usual suite of arithmetic operators (`+`, `-`, `*`, '/' etc.) and comparison operators (`==`, `!=`,  `<`, `>`, `<=`, `>=`).

The types `Int` and `Nat` are unbounded, meaning their values can grow to arbitrary size, limited only by memory. 

The type `Nat` is a subtype of `Int`, so you can always supply a value of type `Nat` where and `Int` is expected but not vice versa.

Motoko also provides bounded, or fixed-size integers and naturals, using a suffix to indicate the size of the type, in bits. 

Thus `Int8`, `Int16` and `Int32` and `Int64` are 8-, 16-, 32- and 64-bit integer types, while `Nat8`, `Nat16`, `Nat32`, and `Nat64` are 8-, 16-, 32- and 64-bit natural types.

An arithmetic operation on a value of a fixed-size type will trap if its result exceeds the bounds of the fixed-size type, either due to overflow or underflow.
For example, `255 : Nat8 + 3` traps, because 258 is too large for a `Nat8`.

Wrapping, non-trapping, versions of the usual arithmetic operations, performing modular arithmetic, are available by suffixing the usual operator with a `%`. For example, `255 : Nat8 +% 3` evaluates to `2`.

The type of an integer or natural constant is determined by the textual context
of the constant, defaulting to `Int` for negative constants and `Nat` for positive ones. 
Otherwise, the type of a constant can be indicated using a type annotation, for example `0 : Nat8`.

One can force a nonnegative constant to be interpreted as an `Int` using an explicit sign, for example  `+1` is the `Int` one.

For convenience, in-place updates of a variable or array element can be written by using a compound assignment operator, combining an arithmetic operation with the assignment operator `:=` . E.g. `x += 1` is short-hand for `x := x + 1` and combines addition `+` with assignment.

Motoko does not provide any implicit conversions between numeric types. Instead, base library functions like `Nat8.toNat`  and `Nat8.fromNat` should be used for explicit conversion.

To illustrate working with numbers, here is an example calculator program that creates a single actor with several public entry-point functions to perform basic arithmetic operations using integers.

## Using integers

```motoko
// This single-cell calculator defines one calculator instruction per
// public entry point (add, sub, mul, div).

// Create a simple Calc actor.
actor Calc {
  var cell : Int = 0;

  // Define functions to add, subtract, multiply, and divide
  public func add(n:Int) : async Int { cell += n; cell };
  public func sub(n:Int) : async Int { cell -= n; cell };
  public func mul(n:Int) : async Int { cell *= n; cell };
  public func div(n:Int) : async ?Int {
    if ( n == 0 ) {
      return null // null indicates div-by-zero error
    } else {
      cell /= n; ?cell
    }
  };

  // Clear the calculator and reset to zero
  public func clearall() : async Int {
    if (cell : Int != 0)
      cell -= cell;
    return cell
  };
 };
```

You might notice that this sample code uses integer (`Int`) data types, enabling you to use positive or negative numbers. If you wanted to restrict the functions in this calculator code to only use positive numbers, you could change the data type to only allow natural (`Nat`) data.

This program supports the following function calls:

-   The `add` function call accepts input and performs addition.

-   The `sub` function call accepts input and performs subtraction.

-   The `mul` function call accepts input and performs multiplication.

-   The `div` function call accepts input and performs division. It also includes code to prevent the program from attempting to divide by zero.

-   The `clearall` function clears the `cell` value stored as the result of previous operations, resetting the `cell` value to zero.

