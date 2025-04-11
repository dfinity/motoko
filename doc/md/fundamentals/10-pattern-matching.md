---
sidebar_position: 10
---

# Pattern matching

Pattern matching allows concise and expressive handling of different data structures in Motoko. It can be used in `switch` expressions to destructure and process values.

Motoko supports several types of patterns:

| Pattern type | Description | Example |
|-------------|-------------|---------|
| Wildcard (`_`) | Matches any value without binding it to a variable. | `switch (x) { case (_) { ... } }` |
| Literal | Matches specific constant values. | `switch (x) { case (0) { ... } case (1) { ... } }` |
| Option (`?T`) | Matches optional values. | `switch (opt) { case (?v) { ... } case (null) { ... } }` |
| Object | Matches object fields. | `switch (obj) { case ({field}) { ... } }` |
| Variant | Matches tagged union types. | `switch (variant) { case (#tag v) { ... } }` |

## Wildcard (`_`)

The wildcard pattern `_` matches any value but does not bind to a variable. It is useful for handling cases where the specific value does not matter.

```motoko no-repl
func processNumber(n: Nat): Text {
    switch (n) {
        case (0) { "Zero" };
        case (1) { "One" };
        case (_) { "Other" };  // Matches any other number
    };
};
```

## Literal

A literal pattern matches specific constant values, such as numbers, booleans, or text.

```motoko no-repl
func describeBool(b: Bool): Text {
    switch (b) {
        case (true) { "It's true" };
        case (false) { "It's false" };
    };
};
```

## Option (`?T`)

Option patterns allow destructuring of `?T` values, matching either `null` or `?someValue`.

```motoko no-repl
import Nat "mo:base/Nat";

func getValue(opt: ?Nat): Text {
    switch (opt) {
        case (?n) { "Value: " # Nat.toText(n) };
        case (null) { "No value" };
    };
};
```

## Object

Objects with named fields can be matched to extract specific properties.

```motoko no-repl
type Person = { name: Text; age: Nat };

func describePerson(person: Person): Text {
    switch (person) {
        case ({ name; age }) { name # " is " # Nat.toText(age) # " years old." };
    };
};
```

## Variant

Variants represent tagged union types, allowing structured handling of multiple possible cases.

```motoko no-repl
type Status = { #ok; #error: Text };

func processStatus(status: Status): Text {
    switch (status) {
        case (#ok) { "Success" };
        case (#error (msg)) { "Error: " # msg };
    };
};
```

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />
