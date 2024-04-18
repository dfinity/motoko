---
sidebar_position: 5
---

# Integers


## Overview

Integers `Int` are primitive values that have bounded and unbounded variants. Integers can be negative numbers such as `-2` or `-1`, or positive numbers such as `1` and `2`.

Integers are unbounded and do not overflow. They use representations that grow to accommodate any finite number.

This page will provide an example calculator program that creates a single actor with several public entry-point functions to perform basic arithmetic operations using integers. 

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

