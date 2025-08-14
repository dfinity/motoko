---
sidebar_position: 15
---

# Type conversions

Conversions are used to transform values between different types to ensure compatibility and ease of manipulation. Common conversions include numeric transformation, such as converting [`Float`](https://internetcomputer.org/docs/motoko/core/Float) or [`Int`](https://internetcomputer.org/docs/motoko/core/Int) to [`Nat`](https://internetcomputer.org/docs/motoko/core/Nat), and text manipulation, like converting [`Text`](https://internetcomputer.org/docs/motoko/core/Text) to [`Float`](https://internetcomputer.org/docs/motoko/core/Float) or encoding [`Text`](https://internetcomputer.org/docs/motoko/core/Text) as a [`Blob`](https://internetcomputer.org/docs/motoko/core/Blob). Arrays and tuples can be converted into structured types, such as records or hashmaps, for better organization. Additionally, time conversions enable transforming `Time.now()` (nanoseconds since 1970) into human-readable date formats, with optional timezone adjustments. These conversions provide flexibility when working with different data types.

## Converting types to `Nat`

Motoko provides methods for converting both [`Float`](https://internetcomputer.org/docs/motoko/core/Float) and [`Int`](https://internetcomputer.org/docs/motoko/core/Int) to [`Nat`](https://internetcomputer.org/docs/motoko/core/Nat). Since [`Nat`](https://internetcomputer.org/docs/motoko/core/Nat) only represents non-negative integers, the conversion must handle cases where the original value is negative or contains a decimal.

### `Float` to `Nat`

A [`Float`](https://internetcomputer.org/docs/motoko/core/Float) can be converted to [`Nat`](https://internetcomputer.org/docs/motoko/core/Nat) using `Float.toInt`, followed by `Int.abs` to ensure a non-negative value.

```motoko
import Float "mo:core/Float";
import Int "mo:core/Int";

func floatToNat(f : Float) : Nat {
    Int.abs(Float.toInt(f));
};

let result1 = floatToNat(42.9);   // 42
let result2 = floatToNat(-15.6);  // 15 (absolute value is taken)
```

### `Int` to `Nat`

[`Int`](https://internetcomputer.org/docs/motoko/core/Int) can be directly converted to [`Nat`](https://internetcomputer.org/docs/motoko/core/Nat) using `Int.abs`, which removes any negative sign.

```motoko
import Int "mo:core/Int";

func intToNat(i : Int) : Nat {
    Int.abs(i);
};

// Example usage
let result3 = intToNat(10);   // 10
let result4 = intToNat(-5);   // 5
```

:::info `Nat` to `Int` conversions functions

`Int.toNat`, `Int.fromNat`, `Nat.toInt`, `Nat.fromInt` all exist in the core package.

:::

## Modifying types

### `Nat8` to `Char`

Motoko allows converting a [`Nat8`](https://internetcomputer.org/docs/motoko/core/Nat8) value to a [`Char`](https://internetcomputer.org/docs/motoko/core/Char), where the [`Nat8`](https://internetcomputer.org/docs/motoko/core/Nat8) value represents a **Unicode scalar value** in the **ASCII range**.

Since [`Nat8`](https://internetcomputer.org/docs/motoko/core/Nat8) is a bounded type `(0–255)`, it must be explicitly widened to [`Nat32`](https://internetcomputer.org/docs/motoko/core/Nat32) before being converted into [`Char`](https://internetcomputer.org/docs/motoko/core/Char).

```motoko no-repl
import Char "mo:core/Char";
import Nat8 "mo:core/Nat8";
import Nat32 "mo:core/Nat32";

persistent actor CharConverter{
  func nat8ToChar(n : Nat8) : Char {
    Char.fromNat32(Nat32.fromNat(Nat8.toNat(n)));
  };
  nat8ToChar(65);  // 'A'
};
```

### `Text` to lowercase

Motoko provides a built-in function `Text.toLower`, which converts all characters in a string to lowercase.

```motoko no-repl
import Text "mo:core/Text";

persistent actor CaseConverter{
  func toLowercaseExample(s : Text) : Text {
    return Text.toLower(s);
  };

    toLowercaseExample("HELLO WORLD");  // "hello world"
};
```

### `Text` to an optional `Blob` for a ledger memo

[`Text`](https://internetcomputer.org/docs/motoko/core/Text) can be converted into a [`Blob`](https://internetcomputer.org/docs/motoko/core/Blob) using `Text.encodeUtf8`. To make it optional (`?Blob`), it can be wrapped in `?`.

```motoko no-repl
import Text "mo:core/Text";

persistent actor TextToBlobConverter {
  func textToOptionalBlob(s : Text) : ?Blob {
    ?Text.encodeUtf8(s);
  };
  textToOptionalBlob("Payment Memo");  // ?Blob
};
```

### `Text` to `Float`

:::info `Text` to `Float` example
Motoko does not have a built-in `Text.toFloat` function, so parsing [`Text`](https://internetcomputer.org/docs/motoko/core/Text) into [`Float`](https://internetcomputer.org/docs/motoko/core/Float) requires handling:

- Integer and decimal parts.
- Scientific notation (`e` or `E`).
- Negative numbers.
- Edge cases (empty input, invalid characters, multiple decimal points, etc.)

[View an example implementation](https://icp.ninja/i?s=wOrN2)
:::

### Non-shared type to shared type

| Non-shared type | Shared equivalent | Conversion method |
|--------------------|----------------------|----------------------|
| Mutable array `[var T]` | Array `[T]` | `Array.freeze(mutArr)` |

### `Time.now()` to date

On ICP, time is measured in nanoseconds since the Unix epoch (January 1, 1970, UTC). The `Time.now()` function returns the current system time in nanoseconds, which must be converted accordingly for date and time.

:::info `Time` to date
Motoko does not provide a built-in function for converting `Time` into a date-time representation. Doing so requires:

- Calculating days and seconds since the Unix epoch (1970-01-01).
- Handling leap years to determine the correct month and day.
- Applying timezone offsets (e.g., subtracting 5 hours for EST).
- Formatting the output as `MM-DD-YYYY HH-MM-SS`.

[View example implementation](https://icp.ninja/i?s=zy4yg).
:::

## Modifying arrays

### `Array` to `Text`

`Text.fromArray` converts a [`Char`](https://internetcomputer.org/docs/motoko/core/Char) into a [`Text`](https://internetcomputer.org/docs/motoko/core/Text). When working with different types, elements must be converted to [`Text`](https://internetcomputer.org/docs/motoko/core/Text) before transformation.

```motoko no-repl
import Text "mo:core/Text";

assert Text.fromArray(['a', 'b', 'c']) == "abc";
```

For arrays containing numbers or other types, each element is converted to [`Text`](https://internetcomputer.org/docs/motoko/core/Text) before joining them into a single string.

```motoko no-repl
import Text "mo:core/Text";
import Array "mo:core/Array";
import Nat "mo:core/Nat";

func arrayOfNatToText(arr : [Nat]) : Text {
  Text.join(" ", Array.map<Nat, Text>(arr, Nat.toText).values())
};

assert arrayOfNatToText([1, 2, 3]) == "1 2 3";
```

### `Array` of tuples to an object

Motoko lacks support for dynamic objects, so an array of tuples is converted into a [record](https://internetcomputer.org/docs/motoko/fundamentals/types/records) or a structured representation.

```motoko no-repl
import HashMap "mo:core/HashMap";
import Text "mo:core/Text";

persistent actor MapConverter {
  func arrayToMap(arr : [(Text, Nat)]) : HashMap.HashMap<Text, Nat> {
    let map = HashMap.HashMap<Text, Nat>(arr.size(), Text.equal, Text.hash);
      for ((key, value) in arr.vals()) {
          map.put(key, value)
      };
      map
  }
};

arrayToMap([("Motoko", 4), ("Ghost", 21)]);
```

To convert an array of tuples `[(Text, Nat)]` into a custom [record](https://internetcomputer.org/docs/motoko/fundamentals/types/records) type, such as `User`, `Array.map` is used to transform each tuple into a structured [record](https://internetcomputer.org/docs/motoko/fundamentals/types/records).

```motoko no-repl
import Array "mo:core/Array";
type User = {
    name : Text;
    age : Nat;
};
persistent actor TupleConverter{
  func tuplesToUsers(arr : [(Text, Nat)]) : [User] {
    Array.map<(Text, Nat), User>(arr, func((name, age)) {
        { name; age }
    });
  }
};

tuplesToUsers([("Motoko", 4), ("Ghost", 21)]);
```
