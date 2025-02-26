---
sidebar_position: 1
---
# Primitive types

Motoko provides several primitive types that form the foundation of all computations. These include numeric types, characters and text, booleans, and floating-point numbers.

## Numeric types

Motoko supports both signed integers and unsigned naturals:

- Signed integers: `Int`, `Int8`, `Int16`, `Int32`, `Int64`
- Unsigned naturals: `Nat`, `Nat8`, `Nat16`, `Nat32`, `Nat64`

The `Int` and `Nat` types prevent overflow and underflow since they dynamically expand or shrink as needed.

While `Nat` can be implicitly converted to `Int`, the reverse is not always true. `Nat` is a subtype of `Int`. In other words, a `Nat` value can be used anywhere an `Int` is expected because `Nat` is always a valid `Int`. This is why implicit conversion from `Nat` to `Int` is allowed:

```motoko norepl
let x: Nat = 5;
let y: Int = x; // Allowed
```

However, an `Int` cannot be directly assigned to a `Nat` if it holds a negative value, as `Nat` only supports non-negative numbers:

```motoko norepl
let x: Int = -5;
let y: Nat = x; // Error
```

To convert a negative `Int` to a `Nat`, the absolute value must be taken explicitly:

```motoko norepl
let x: Int = -5;
let y: Nat = Int.abs(x); // Allowed, y = 5
```

Fixed-size numeric types (`Int8`, `Nat32`, etc.) support additional operations, including bitwise shifts:

```motoko norepl
let x: Nat32 = 0xA; // 10 in hexadecimal
let y = Nat32.bitshiftLeft(x, 2); // 0x28 (40 in decimal)
```

## Char and text

`Char` represents a single Unicode scalar value, while `Text` represents a sequence of characters.

```motoko norepl
let letter: Char = 'A';
let codePoint = Char.toNat32(letter); // 65

let word: Text = "Motoko";
let uppercase = Text.toUppercase(word); // "MOTOKO"

let modified = Text.replace("hello world", "world", "Motoko"); // "hello Motoko"
let words = Text.split("apple,banana,cherry", ","); // ["apple", "banana", "cherry"]
```

## Bool

The `Bool` type represents boolean values, `true` or `false`, and supports logical operations.

```motoko norepl
let flag: Bool = Bool.logor(true, false); // true
let opposite = Bool.lognot(flag); // false

let isEqual = Bool.equal(true, false); // false
let comparison = Bool.compare(true, false); // #greater
```

## Float

`Float` is a 64-bit floating-point type that provides mathematical operations.

```motoko norepl
imoprt Float "mo:base/Float
let pi = Float.pi;
let radius: Float = 2.5;
let area = Float.pow(radius, 2) * pi; // Area of a circle

let rounded = Float.floor(4.8); // 4.0
let trigValue = Float.sin(Float.pi / 2); // 1.0
```

## References

- [Int](https://internetcomputer.org/docs/current/motoko/main/base/int)
- [Nat](https://internetcomputer.org/docs/current/motoko/main/base/nat)
- [Bool](https://internetcomputer.org/docs/current/motoko/main/base/bool)
- [Char](https://internetcomputer.org/docs/current/motoko/main/base/char)
- [Text](https://internetcomputer.org/docs/current/motoko/main/base/text)
- [Float](https://internetcomputer.org/docs/current/motoko/main/base/float)
