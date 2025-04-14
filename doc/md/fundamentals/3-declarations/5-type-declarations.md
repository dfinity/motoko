---
sidebar_position: 5
---

# Type declarations

A type declaration are used for defining custom types that improve readability, reusability, and structure in the code. They can represent records, variants, objects, or parameterized (generic) types. Motoko enforces productivity and non-expansiveness in type declarations to ensure well-formed, valid types.

## Defining a type

The `type` keyword assigns a name to a type and makes it reusable.

```motoko no-repl
// An alias for Nat
type Age = Nat;

// An alias for Text
type Username = Text;
```

These types can then be used in function definitions.

```motoko no-repl
func greet(name: Username, age: Age) : Text {
    "Hello, " # name # "! You are " # Nat.toText(age) # " years old."
}
```

## Record types

A type can represent a structured [record](https://internetcomputer.org/docs/motoko/fundamentals/types/records) with labeled fields.

```motoko no-repl
// A reusable record
type Ghost = {
    firstName: Text;
    lastName: Text;
    age: Nat;
};

// An instance of Ghost
let motoko: Ghost = {
    firstName = "Motoko";
    lastName = "Sentinels";
    age = 30;
};
```

## Variant types

A type can also define variants, representing different possible states.

```motoko no-repl
// Allows only one of its variants at a time.
type Status = {
    #Active;
    #Inactive;

// Carries an additional Text value.
    #Banned : Text;
};

let userStatus: Status = #Active;
let bannedUser: Status = #Banned("Violation of rules");
```

## Parameterized (generic) types

Type declarations can be parameterized to work with multiple types.

```motoko no-repl
// `Box<T> is a generic type where T represents any type.
type Box<T> = {
    value: T;
};

// numberBox stores a Nat and textBox stores a Text.
let numberBox: Box<Nat> = { value = 42 };
let textBox: Box<Text> = { value = "Hello" };
```

This ensures flexibility while keeping type safety.

## Recursive types and productivity

Motoko allows recursive type definitions, provided they are productive.

### Productive recursive type

```motoko no-repl
type List<T> = ?(T, List<T>);
```

`List<T>` defines a linked list structure. Each list node contains a value (`T`) and another `List<T>` or `null` (end of the list). This type is non-expansive and accepted.

### Non-productive recursive type

```motoko no-repl
type C = C; // This definition infinitely refers to itself
```

This type will never resolve to a concrete type. It is non-productive and rejected.

## Expansiveness in type definitions

Motoko enforces non-expansiveness to prevent type definitions from expanding indefinitely.

### Non-expansive

```motoko no-repl
type List<T> = ?(T, List<T>);
```

Expands without introducing a larger type.

### Expansive

```motoko no-repl
type Seq<T> = ?(T, Seq<[T]>);
```

Expands by wrapping `T` inside `[T]`, growing the type. This is expansive and not allowed.

## Resources

- [`Record`](https://internetcomputer.org/docs/motoko/fundamentals/types/records)
- [`Variant`](https://internetcomputer.org/docs/motoko/fundamentals/types/variants)

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />