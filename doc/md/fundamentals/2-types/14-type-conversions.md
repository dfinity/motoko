---
sidebar_position: 14
---

# Type conversions

Conversions are used to transform values between different types to ensure compatibility and ease of manipulation. Common conversions include numeric transformation, such as converting [`Float`](https://internetcomputer.org/docs/motoko/base/Float) or [`Int`](https://internetcomputer.org/docs/motoko/base/Int) to [`Nat`](https://internetcomputer.org/docs/motoko/base/Nat), and text manipulation, like converting [`Text`](https://internetcomputer.org/docs/motoko/base/Text) to [`Float`](https://internetcomputer.org/docs/motoko/base/Float) or encoding [`Text`](https://internetcomputer.org/docs/motoko/base/Text) as a [`Blob`](https://internetcomputer.org/docs/motoko/base/Blob). Arrays and tuples can be converted into structured types, such as records or hashmaps, for better organization. Additionally, time conversions enable transforming `Time.now()` (nanoseconds since 1970) into human-readable date formats, with optional timezone adjustments. These conversions provide flexibility when working with different data types.

## Converting types to `Nat`

Motoko provides methods for converting both [`Float`](https://internetcomputer.org/docs/motoko/base/Float) and [`Int`](https://internetcomputer.org/docs/motoko/base/Int) to [`Nat`](https://internetcomputer.org/docs/motoko/base/Nat). Since [`Nat`](https://internetcomputer.org/docs/motoko/base/Nat) only represents non-negative integers, the conversion must handle cases where the original value is negative or contains a decimal.

### `Float` to `Nat`

A [`Float`](https://internetcomputer.org/docs/motoko/base/Float) can be converted to [`Nat`](https://internetcomputer.org/docs/motoko/base/Nat) using `Float.toInt`, followed by `Int.abs` to ensure a non-negative value.

```motoko no-repl
import Float "mo:base/Float";
import Int "mo:base/Int";

func floatToNat(f: Float): Nat {
    return Nat.Int(Float.toInt(f));
};

let result1 = floatToNat(42.9);   // 42
let result2 = floatToNat(-15.6);  // 15 (absolute value is taken)
```

### `Int` to `Nat`

[`Int`](https://internetcomputer.org/docs/motoko/base/Int) can be directly converted to [`Nat`](https://internetcomputer.org/docs/motoko/base/Nat) using `Int.abs`, which removes any negative sign.

```motoko no-repl
import Int "mo:base/Int";

func intToNat(i: Int): Nat {
    return Int.abs(i);
};

// Example usage
let result3 = intToNat(10);   // 10
let result4 = intToNat(-5);   // 5
```

## Modifying types

### `Nat8` to `Char`

Motoko allows converting a [`Nat8`](https://internetcomputer.org/docs/motoko/base/Nat8) value to a [`Char`](https://internetcomputer.org/docs/motoko/base/Char), where the [`Nat8`](https://internetcomputer.org/docs/motoko/base/Nat8) value represents a **unicode scalar value** in the **ASCII range**.

Since [`Nat8`](https://internetcomputer.org/docs/motoko/base/Nat8) is a bounded type `(0–255)`, it must be explicitly widened to [`Nat32`](https://internetcomputer.org/docs/motoko/base/Nat32) before being converted into [`Char`](https://internetcomputer.org/docs/motoko/base/Char).

```motoko no-repl
import Char "mo:base/Char";
import Nat8 "mo:base/Nat8";
import Nat32 "mo:base/Nat32";

func nat8ToChar(n: Nat8): Char {
    return Char.fromNat32(Nat32.fromNat(Nat8.toNat(n)));
};

let charA = nat8ToChar(65);  // 'A'
```

### `Text` to lowercase

Motoko provides a built-in function `Text.toLowercase`, which converts all characters in a string to lowercase.

```motoko no-repl
import Text "mo:base/Text";

func toLowercaseExample(s: Text): Text {
    return Text.toLowercase(s);
};

let result1 = toLowercaseExample("HELLO WORLD");  // "hello world"
```

### `Text` to an optional `Blob` for a ledger memo

[`Text`](https://internetcomputer.org/docs/motoko/base/Text) can be converted into a [`Blob`](https://internetcomputer.org/docs/motoko/base/Blob) using `Text.encodeUtf8`. To make it optional (`?Blob`), it can be wrapped in `?`.

```motoko no-repl
import Text "mo:base/Text";

func textToOptionalBlob(s: Text): ?Blob {
    return ?Text.encodeUtf8(s);
};

let memo1 = textToOptionalBlob("Payment Memo");  // ?Blob
```

### `Text` to `Float`

<!--This was taken from my JSON library for parsing JSON floats. This could potentially be its own page-->

Motoko does not have a built-in `Text.toFloat` function, so parsing [`Text`](https://internetcomputer.org/docs/motoko/base/Text) into [`Float`](https://internetcomputer.org/docs/motoko/base/Float) requires handling:

- Integer and decimal parts.
- Scientific notation (`e` or `E`).
- Negative numbers.
- Edge cases (empty input, invalid characters, multiple decimal points, etc.)

Below is a robust implementation:

```motoko no-repl
import Debug "mo:base/Debug";
import Text "mo:base/Text";
import Char "mo:base/Char";
import Float "mo:base/Float";
import Int "mo:base/Int";
import Int32 "mo:base/Int32";
import Iter "mo:base/Iter";
  func charToInt(c: Char): Int {
    Int32.toInt(Int32.fromNat32(Char.toNat32(c) - 48));
  }

  func textToFloat(text : Text) : ?Float {
    var integer : Int = 0;
    var fraction : Float = 0;
    var isNegative = false;
    var position: Nat = 0;
    let chars = text.chars();

    if (Text.size(text) == 0) {
      return null
    };
    let firstchar = Text.toArray(text)[0];

    if(firstchar == '-' and text.size()== 1){
      return null;
    };
    if (firstchar == 'e' or firstchar == 'E'){
      return null
    };

    switch (chars.next()) {
      case (?'-') {
        isNegative := true;
        position += 1
      };
      case (?'+') {
        position += 1
      };
      case (?'.') {
        position += 1;
        switch (chars.next()) {
          case (?d) if (Char.isDigit(d)) {
            fraction := 0.1 * Float.fromInt(charToInt(d));
            position += 1
          };
          case (_) {return null}
        }
      };
      case (?d) if (Char.isDigit(d)) {
        integer := charToInt(d);
        position += 1
      };
      case (_) {return null}
    };

    var hasDigits = position > 0;
    label integer loop {
      switch (chars.next()) {
        case (?d) {
          if (Char.isDigit(d)) {
            integer := integer * 10 + charToInt(d);
            position += 1;
            hasDigits := true
          } else if (d == '.') {
            position += 1;
            break integer
          } else if (d == 'e' or d == 'E') {
            position += 1;
            if (not hasDigits) {
              return null
            };

            var expResult = parseExponent(chars);
            switch (expResult) {
              case (null) {
                return null;
              };
              case (?(expValue, _)) {
                // Calculate final value with exponent
                let base = Float.fromInt(if (isNegative) -integer else integer) +
                (if (isNegative) -fraction else fraction);
                let multiplier = Float.pow(10, Float.fromInt(expValue));
                return ?(base * multiplier)
              }
            }
          } else {
            return null
          }
        };
        case (null) {
          if (not hasDigits) {
            return null;
          };
          return ?(Float.fromInt(if (isNegative) -integer else integer))
        }
      }
    };

    var fractionMultiplier : Float = 0.1;
    var hasFractionDigits = false;

    label fraction loop {
      switch (chars.next()) {
        case (?d) {
          if (Char.isDigit(d)) {
            fraction += fractionMultiplier * Float.fromInt(charToInt(d));
            fractionMultiplier *= 0.1;
            position += 1;
            hasFractionDigits := true
          } else if (d == 'e' or d == 'E') {
            position += 1;

            if (not (hasDigits or hasFractionDigits)) {
              return null
            };

            // Handle exponent part
            var expResult = parseExponent(chars);
            switch (expResult) {
              case (null) {
                return null; // Invalid exponent format
              };
              case (?(expValue, _)) {
                // Calculate final value with exponent
                let base = Float.fromInt(if (isNegative) -integer else integer) +
                (if (isNegative) -fraction else fraction);
                let multiplier = Float.pow(10, Float.fromInt(expValue));
                return ?(base * multiplier)
              }
            }
          } else {
            return null
          }
        };
        case (null) {
          // End of input - return complete number
          let result = Float.fromInt(if (isNegative) -integer else integer) +
          (if (isNegative) -fraction else fraction);
          return ?result
        }
      }
    };

    return null;
  };

  func parseExponent(chars : Iter.Iter<Char>) : ?(Int, Nat) {
    var exponent : Int = 0;
    var expIsNegative = false;
    var position = 0;
    var hasDigits = false;

    // Parse optional sign or first digit
    switch (chars.next()) {
      case (?d) {
        if (d == '-') {
          expIsNegative := true;
          position += 1
        } else if (d == '+') {
          position += 1
        } else if (Char.isDigit(d)) {
          exponent := charToInt(d);
          position += 1;
          hasDigits := true
        } else {
          return null
        }
      };
      case (null) {return null};
    };

    label exponent loop {
      switch (chars.next()) {
        case (?d) {
          if (Char.isDigit(d)) {
            exponent := exponent * 10 + charToInt(d);
            position += 1;
            hasDigits := true
          } else {
            return null;
          }
        };
        case (null) {
          if (not hasDigits) {
            return null;
          };
          return ?(if (expIsNegative) -exponent else exponent, position)
        }
      }
    };

    return null;
  };
```

### Non-shared type to shared type

| Non-shared type | Shared equivalent | Conversion method |
|--------------------|----------------------|----------------------|
| `Buffer<T>` | `[T]` | `Buffer.toArray(buffer)` |
| `HashMap<K, V>` | `[(K, V)]` | `Iter.toArray(hashmap.entries())` |

### `Time.now()` to date

On ICP, time is measured in nanoseconds since the Unix epoch (January 1, 1970, UTC). The `Time.now()` function returns the current system time in nanoseconds, which must be converted accordingly for date and time.

This section demonstrates how to extract the year, month, day, hour, minute, and second from `Time.now()` by computing the number of days and seconds elapsed since 1970. It accounts for leap years when determining the month and day.

Additionally, a method for applying a timezone offset is provided, allowing adjustments such as converting UTC to Eastern Standard Time (EST) by subtracting 5 hours. The final output is formatted as `MM-DD-YYYY HH-MM-SS` for readability.

```motoko no-repl
import Time "mo:base/Time";
import Int "mo:base/Int";
import Text "mo:base/Text";

actor {
    public func timesToDate() : async Text {

        let (year, month, day, hour, minute, second) = timestampToDateTime(system_time());

        // Format as MM-DD-YYYY HH-MM-SS
        //UTC Time
        return formatNumber(month) # "-" #
               formatNumber(day) # "-" #
               Int.toText(year) # "-" #
               formatNumber(hour) # " " #
               formatNumber(minute) # "-" #
               formatNumber(second);
    };

    func timestampToDateTime(timestamp : Int) : (Int, Int, Int, Int, Int, Int) {
        // Get seconds since epoch
        let seconds = timestamp;

        // Calculate time components
        let secondsInDay = seconds % 86400;
        let hour = secondsInDay / 3600;
        let minute = (secondsInDay % 3600) / 60;
        let second = secondsInDay % 60;

        let daysSinceEpoch = seconds / 86400;

        let dateResult = calculateDate(daysSinceEpoch);
        let year = dateResult.0;
        let month = dateResult.1;
        let day = dateResult.2;

        return (year, month, day, hour, minute, second);
    };

    func calculateDate(days : Int) : (Int, Int, Int) {
        var year = 1970;
        var remainingDays = days;

        // Calculate the year
        while (remainingDays >= daysInYear(year)) {
            remainingDays -= daysInYear(year);
            year += 1;
        };

        // Calculate the month
        let monthLengths = getMonthLengths(year);
        var month = 1;

        label search for (daysInMonth in monthLengths.vals()) {
            if (remainingDays < daysInMonth) {
                break search;
            };
            remainingDays -= daysInMonth;
            month += 1;
        };

        let day = remainingDays + 1;

        return (year, month, day);
    };

    func daysInYear(year : Int) : Int {
        if (isLeapYear(year)) { 366 } else { 365 };
    };

    func system_time() : Int {
        Time.now() / 1_000_000_000;
    };

    func isLeapYear(year : Int) : Bool {
        (year % 4 == 0 and year % 100 != 0) or (year % 400 == 0);
    };

    func getMonthLengths(year : Int) : [Int] {
        [ 31, if (isLeapYear(year)) { 29 } else { 28 }, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, ];
    };

    func formatNumber(n : Int) : Text {
        if (n < 10) { "0" # Int.toText(n) } else { Int.toText(n) };
    };
};
```

To account for the timezone difference, apply an offset. For example, adjust the time by subtracting 5 hours (assuming EST):

```motoko no-repl
let adjustedTime = system_time - (5 * 3600);
```

## Modifying arrays

### `Array` to `Text`

`Text.fromArray` converts a [`Char`](https://internetcomputer.org/docs/motoko/base/Char) into a [`Text`](https://internetcomputer.org/docs/motoko/base/Text). When working with different types, elements must be converted to [`Text`](https://internetcomputer.org/docs/motoko/base/Text) before transformation.

```motoko no-repl
import Text "mo:base/Text";
import Array "mo:base/Array";

func arrayToText(arr: [Char]): Text {
    return Text.fromArray(arr);
};
```

For arrays containing numbers or other types, each element is converted to [`Text`](https://internetcomputer.org/docs/motoko/base/Text) before joining them into a single string.

```motoko no-repl
import Text "mo:base/Text";
import Array "mo:base/Array";

public func arrayOfNatToText(arr: [Nat]): async Text {
    return Text.join(" ", Array.map<Nat, Text>(arr, Nat.toText).vals());
};
```

### `Array` of tuples to an object

Motoko lacks support for dynamic objects, so an array of tuples is converted into a [record](https://internetcomputer.org/docs/motoko/fundamentals/types/records) or a structured representation.

```motoko no-repl
import HashMap "mo:base/HashMap";

func arrayToMap(arr: [(Text, Nat)]): HashMap.HashMap<Text, Nat> {
    let map = HashMap.HashMap<Text, Nat>(arr.size(), Text.equal, Text.hash);
    for ((key, value) in arr.vals()) {
        map.put(key, value);
    };
    return map;
};

```

To convert an array of tuples `[(Text, Nat)]` into a custom [record](https://internetcomputer.org/docs/motoko/fundamentals/types/records) type, such as `User`, `Array.map` is used to transform each tuple into a structured [record](https://internetcomputer.org/docs/motoko/fundamentals/types/records).

```motoko no-repl
import Array "mo:base/Array";
type User = {
    name: Text;
    age: Nat;
};

func tuplesToUsers(arr: [(Text, Nat)]): [User] {
    return Array.map<(Text, Nat), User>(arr, func((name, age)) {
        { name = name; age = age }
    });
};
```

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />