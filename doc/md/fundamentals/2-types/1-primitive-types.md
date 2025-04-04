---
sidebar_position: 1
---

# Primitive types

Motoko provides several primitive types that form the foundation of all computations. These include numeric types, characters and text, booleans, and floating-point numbers.

## Numeric types

Motoko supports both signed integers and unsigned naturals. Signed numbers can represent all numbers, positive and negative, while unsigned integers can only represent 0 and positive numbers. Natural numbers are unsigned integers.

- Signed integers: [`Int`](/docs/motoko/base/Int), `Int8`, `Int16`, `Int32`, `Int64`
- Unsigned naturals: [`Nat`](/docs/motoko/base/Nat), `Nat8`, `Nat16`, `Nat32`, `Nat64`

The [`Int`](/docs/motoko/base/Int) and [`Nat`](/docs/motoko/base/Nat) types prevent overflow and underflow since they dynamically expand or shrink as needed.

While [`Nat`](/docs/motoko/base/Nat) can be implicitly converted to [`Int`](/docs/motoko/base/Int), the reverse is not always true. [`Nat`](/docs/motoko/base/Nat) is a subtype of [`Int`](/docs/motoko/base/Int). In other words, a [`Nat`](/docs/motoko/base/Nat) value can be used anywhere an [`Int`](/docs/motoko/base/Int) is expected because [`Nat`](/docs/motoko/base/Nat) is always a valid [`Int`](/docs/motoko/base/Int). This is why implicit conversions from [`Nat`](/docs/motoko/base/Nat) to [`Int`](/docs/motoko/base/Int) are allowed:

```motoko no-repl
let x: Nat = 5;
let y: Int = x; // Allowed
```

However, an [`Int`](/docs/motoko/base/Int) cannot be directly assigned to a [`Nat`](/docs/motoko/base/Nat) if it holds a negative value, as [`Nat`](/docs/motoko/base/Nat) only supports non-negative numbers:

```motoko no-repl
let x: Int = -5;
let y: Nat = x; // Error
```

To convert a negative [`Int`](/docs/motoko/base/Int) to a [`Nat`](/docs/motoko/base/Nat), the absolute value must be taken explicitly:

```motoko no-repl
let x: Int = -5;
let y: Nat = Int.abs(x); // Allowed, y = 5
```

Fixed-size numeric types (`Int8`, `Nat32`, etc.) support additional operations, including bitwise shifts:

```motoko no-repl
let x: Nat32 = 0xA; // 10 in hexadecimal
let y = Nat32.bitshiftLeft(x, 2); // 0x28 (40 in decimal)
```

## Char and Text

`Char` represents a single Unicode scalar value, while [`Text`](/docs/motoko/base/Text) represents a sequence of characters.

```motoko no-repl
let letter: Char = 'A';
let codePoint = Char.toNat32(letter); // 65

let word: Text = "Motoko";
let uppercase = Text.toUppercase(word); // "MOTOKO"

let modified = Text.replace("hello world", "world", "Motoko"); // "hello Motoko"
let words = Text.split("apple,banana,cherry", ","); // ["apple", "banana", "cherry"]
```

## Bool

The [`Bool`](/docs/motoko/base/Bool) type represents boolean values, `true` or `false`, and supports logical operations.

```motoko no-repl
let flag: Bool = Bool.logor(true, false); // true
let opposite = Bool.lognot(flag); // false

let isEqual = Bool.equal(true, false); // false
let comparison = Bool.compare(true, false); // #greater
```

## Float

[`Float`](/docs/motoko/base/Float) is a 64-bit floating-point type that provides mathematical operations.

```motoko no-repl
import Float "mo:base/Float
let pi = Float.pi;
let radius: Float = 2.5;
let area = Float.pow(radius, 2) * pi; // Area of a circle

let rounded = Float.floor(4.8); // 4.0
let trigValue = Float.sin(Float.pi / 2); // 1.0
```

## References

- [Int](/docs/motoko/base/int)
- [Nat](/docs/motoko/base/nat)
- [Bool](/docs/motoko/base/bool)
- [Char](/docs/motoko/base/char)
- [Text](/docs/motoko/base/text)
- [Float](/docs/motoko/base/float)

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />