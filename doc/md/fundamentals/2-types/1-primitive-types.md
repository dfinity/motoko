---
sidebar_position: 1
---

# Primitive types

Motoko provides several primitive types that form the foundation of all computations. These include numeric types, characters and text, booleans, and floating-point numbers.

## Numeric types

Motoko supports both signed integers and unsigned naturals. Signed numbers can represent all numbers, positive and negative, while unsigned integers can only represent 0 and positive numbers. Natural numbers are unsigned integers.

- Signed integers: [`Int`](https://internetcomputer.org/docs/motoko/base/Int), [`Int8`](https://internetcomputer.org/docs/motoko/base/Int8), [`Int16`](https://internetcomputer.org/docs/motoko/base/Int16), [`Int32`](https://internetcomputer.org/docs/motoko/base/Int32), [`Int64`](https://internetcomputer.org/docs/motoko/base/Int64)
- Unsigned naturals: [`Nat`](https://internetcomputer.org/docs/motoko/base/Nat), [`Nat8`](https://internetcomputer.org/docs/motoko/base/Nat8), [`Nat16`](https://internetcomputer.org/docs/motoko/base/Nat16), [`Nat32`](https://internetcomputer.org/docs/motoko/base/Nat32), [`Nat64`](https://internetcomputer.org/docs/motoko/base/Nat64)

The [`Int`](https://internetcomputer.org/docs/motoko/base/Int) and [`Nat`](https://internetcomputer.org/docs/motoko/base/Nat) types prevent overflow and underflow since they dynamically expand or shrink as needed.

While [`Nat`](https://internetcomputer.org/docs/motoko/base/Nat) can be implicitly converted to [`Int`](https://internetcomputer.org/docs/motoko/base/Int), the reverse is not always true. [`Nat`](https://internetcomputer.org/docs/motoko/base/Nat) is a subtype of [`Int`](https://internetcomputer.org/docs/motoko/base/Int). In other words, a [`Nat`](https://internetcomputer.org/docs/motoko/base/Nat) value can be used anywhere an [`Int`](https://internetcomputer.org/docs/motoko/base/Int) is expected because [`Nat`](https://internetcomputer.org/docs/motoko/base/Nat) is always a valid [`Int`](https://internetcomputer.org/docs/motoko/base/Int). This is why implicit conversions from [`Nat`](https://internetcomputer.org/docs/motoko/base/Nat) to [`Int`](https://internetcomputer.org/docs/motoko/base/Int) are allowed.

```motoko no-repl
let x: Nat = 5;
let y: Int = x; // Allowed
```

However, an [`Int`](https://internetcomputer.org/docs/motoko/base/Int) cannot be directly assigned to a [`Nat`](https://internetcomputer.org/docs/motoko/base/Nat) if it holds a negative value, as [`Nat`](https://internetcomputer.org/docs/motoko/base/Nat) only supports non-negative numbers.

```motoko no-repl
let x: Int = -5;
let y: Nat = x; // Error
```

To convert a negative [`Int`](https://internetcomputer.org/docs/motoko/base/Int) to a [`Nat`](https://internetcomputer.org/docs/motoko/base/Nat), the absolute value must be taken explicitly.

```motoko no-repl
let x: Int = -5;
let y: Nat = Int.abs(x); // Allowed, y = 5
```

Fixed-size numeric types ([`Int8`](https://internetcomputer.org/docs/motoko/base/Int8), [`Nat32`](https://internetcomputer.org/docs/motoko/base/Nat32), etc.) support additional operations, including bitwise shifts.

```motoko no-repl
let x: Nat32 = 0xA; // 10 in hexadecimal
let y = Nat32.bitshiftLeft(x, 2); // 0x28 (40 in decimal)
```

## Char and Text

`Char` represents a single Unicode scalar value, while [`Text`](https://internetcomputer.org/docs/motoko/base/Text) represents a sequence of characters.

```motoko no-repl
let letter: Char = 'A';
let codePoint = Char.toNat32(letter); // 65

let word: Text = "Motoko";
let uppercase = Text.toUppercase(word); // "MOTOKO"

let modified = Text.replace("hello world", "world", "Motoko"); // "hello Motoko"
let words = Text.split("apple,banana,cherry", ","); // ["apple", "banana", "cherry"]
```

## Bool

The [`Bool`](https://internetcomputer.org/docs/motoko/base/Bool) type represents boolean values, `true` or `false`, and supports logical operations.

```motoko no-repl
let flag: Bool = Bool.logor(true, false); // true
let opposite = Bool.lognot(flag); // false

let isEqual = Bool.equal(true, false); // false
let comparison = Bool.compare(true, false); // #greater
```

## Float

[`Float`](https://internetcomputer.org/docs/motoko/base/Float) is a 64-bit floating-point type that provides mathematical operations.

```motoko no-repl
import Float "mo:base/Float
let pi = Float.pi;
let radius: Float = 2.5;
let area = Float.pow(radius, 2) * pi; // Area of a circle

let rounded = Float.floor(4.8); // 4.0
let trigValue = Float.sin(Float.pi / 2); // 1.0
```

## Resources

- [`Int`](https://internetcomputer.org/docs/motoko/base/int)
- [`Nat`](https://internetcomputer.org/docs/motoko/base/nat)
- [`Bool`](https://internetcomputer.org/docs/motoko/base/bool)
- [`Char`](https://internetcomputer.org/docs/motoko/base/char)
- [`Text`](https://internetcomputer.org/docs/motoko/base/text)
- [`Float`](https://internetcomputer.org/docs/motoko/base/float)

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />