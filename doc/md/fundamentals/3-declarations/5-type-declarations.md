---
sidebar_position: 5
---

# Type declarations

Type declarations are used for defining custom types that improve readability, reusability, and structure of the code. They can represent records, variants, objects, or parameterized (generic) types. Motoko enforces productivity and non-expansiveness in type declarations to ensure well-formed, valid types.

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
func greet(name : Username, age : Age) : Text {
    "Hello, " # name # "! You are " # Nat.toText(age) # " years old."
}
```

## Record types

In Motoko, a type can define a structured [record](https://internetcomputer.org/docs/motoko/fundamentals/types/records) with labeled fields. Each field has a specific type, and you can access them using dot notation. Records are useful for organizing related data clearly and safely.

```motoko no-repl
// A reusable record
type Ghost = {
    firstName : Text;
    lastName : Text;
    age : Nat;
};

// An instance of Ghost
let motoko : Ghost = {
    firstName = "Motoko";
    lastName = "Sentinels";
    age = 30;
};
```

## Variant types

A type can also define variants, which represent different possible states or alternatives. Variants allow a value to be one of several labeled options, making it easy to handle data that can take multiple forms.

```motoko no-repl
// Allows only one of its variants at a time.
type Status = {
    #Active;
    #Inactive;

// Carries an additional Text value.
    #Banned : Text;
};

let userStatus : Status = #Active;
let bannedUser : Status = #Banned("Violation of rules");
```

## Parameterized (generic) types

Type declarations can be parameterized, allowing them to work flexibly with multiple types while ensuring type safety. This lets you create generic and reusable type definitions that adapt to different data types as needed.

```motoko no-repl
// `Box<T> is a generic type where T represents any type.
type Box<T> = {
    value: T;
};

// numberBox stores a Nat and textBox stores a Text.
let numberBox : Box<Nat> = { value = 42 };
let textBox : Box<Text> = { value = "Hello" };
```

## Recursive types and productivity

Motoko allows recursive type definitions as long as they are productive. This means that any recursion in a type must pass through a constructor (such as an option (`?`), a variant, or a record field) before referring back to itself.

#### Productive recursive type example

```motoko no-repl
type List<T> = ?(T, List<T>);
```

`List<T>` defines a linked list where each node holds a value of type `T` and points to either another `List<T>` or `null` to mark the end. Since the recursion passes through a constructor (`?`), this type is productive and accepted by the compiler.

#### Non-productive recursive type example

```motoko no-repl
type C = C; // This definition infinitely refers to itself
```

This type never resolves to a concrete value because it lacks a constructor in its recursion; it is non-productive and rejected by the compiler.

## Expansiveness in type definitions

Motoko enforces non-expansiveness to ensure type definitions do not expand indefinitely, keeping types well-formed and preventing infinite or unmanageable type structures.

#### Non-expansive

```motoko no-repl
/// Expands without introducing a larger type.
type List<T> = ?(T, List<T>);
```

#### Expansive

```motoko no-repl
///Expands by wrapping T inside [T], growing the type. This is expansive and not allowed.
type Seq<T> = ?(T, Seq<[T]>);
```

## Resources

- [`Record`](https://internetcomputer.org/docs/motoko/fundamentals/types/records)
- [`Variant`](https://internetcomputer.org/docs/motoko/fundamentals/types/variants)

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />