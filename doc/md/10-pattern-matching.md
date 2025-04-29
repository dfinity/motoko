---
sidebar_position: 10
---

# Pattern matching

Pattern matching allows concise and expressive handling of different data structures in Motoko. It can be used in `switch` expressions to destructure and process values.

Motoko supports several types of patterns:

| Pattern type | Description | Example |
|-------------|-------------|---------|
| Wildcard `_` | Matches any value without binding it to a variable. | `switch x { case (_) { ... } }` |
| Literal | Matches specific constant values. | `switch x { case (0) { ... } case (1) { ... } }` |
| Option (`?T`) | Matches optional values. | `switch (opt) { case (?v) { ... } case (null) { ... } }` |
| Object | Matches object fields. | `switch (obj) { case ({field}) { ... } }` |
| Variant | Matches tagged union types. | `switch (variant) { case (#tag v) { ... } }` |
| Let-else | Matches a pattern, otherwise executes else block and exits execution. |  `let ?x = pattern() else { ... };` |
| Tuples |  Matches `n`-tuple value against an `n`-tuple of patterns. | `let x = ( ... , ... );` |

## Wildcard `_`

The wildcard pattern `_` matches any value but does not bind to a variable. It is useful for handling cases where the specific value does not matter.

```motoko no-repl
func processNumber(n : Nat) : Text {
    switch (n) {
        case 0 { "Zero" };
        case 1 { "One" };
        case _ { "Other" };  // Matches any other number
    };
};
```

## Literal

A literal pattern matches specific constant values, such as numbers, booleans, or text.

```motoko no-repl
func describeBool(b : Bool) : Text {
    switch b {
        case true { "It's true" };
        case false { "It's false" };
    };
};
```

## Option (`?T`)

Option patterns allow destructuring of `?T` values, matching either `null` or `?someValue`.

```motoko no-repl
import Nat "mo:base/Nat";

func getValue(opt : ?Nat) : Text {
    switch opt {
        // Parentheses required around `?n` 
        case (?n) { "Value: " # Nat.toText(n) };
        case null { "No value" };
    };
};
```

## Object

Objects with named fields can be matched to extract specific properties. Records use the same pattern.

```motoko no-repl
func describePerson(person : Person) : Text {
    let { name; age } = person;
    name # " is " # Nat.toText(age) # " years old."
};
```

## Variant

Variants represent tagged union types, allowing structured handling of multiple possible cases.

```motoko no-repl
type Status = { #ok; #error : Text };

func processStatus(status : Status) : Text {
    switch status {
        case (#ok) { "Success" };
        case (#error message) { "Error: " # message };
    };
};
```

## Let-else

`let-else` works similarly to the [`let` declaration](https://internetcomputer.org/docs/motoko/language-manual#let-declaration), except there is an `else` block that gets executed if the pattern match fails.

```motoko
let ?x = pattern() else { Debug.print "Pattern match failed"; return };
```

If `pattern()` returns `null` and `null` doesn't match the pattern `?x`, and `Pattern match failed` is printed. Otherwise, the `Nat` is bound to `x` and execution continues.

`let-else` is useful for avoiding deeply nested switch statements. Instead, you can simply use something like:

```motoko
let ?x = pattern1() else { return #err "Pattern 1 failed" };
let ?y = pattern2() else { return #err "Pattern 2 failed" };
let ?z = pattern3() else { return #err "Pattern 3 failed" };

return #ok x + y + x
```

## Tuples 

Tuples can be used to match a `n`-tuple value against an `n`-tuple of patterns. Both the tuple and the pattern must have the same number of items.

Pattern matching fails when one pattern does not match the corresponding item of the tuple. Pattern matching is successful if every pattern matches the corresponding tuple value.

```
func processData((name, age) : (Text, Nat)) : Text {
  name # " is " # Nat.toText(age) # " years old"
};

let result = processData(("Motoko", 25));
```


<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />
