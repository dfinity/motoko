---
sidebar_position: 3
---

# Object and record declarations

In Motoko, records and objects are both used to group related data, but they differ in purpose and capabilities.

Objects are constructs that support mutable fields, can contain functions, and allow for visibility modifiers like public and private, enabling encapsulation and modular design.

Records are simple data structures that consist of named fields holding values. They cannot contain functions. They are primarily used for organizing static, structured data and do not support behavior or mutable state.

While records are ideal for lightweight data representation, objects are better suited for scenarios requiring state management and method definitions.

## Objects

An object declaration is a structured collection of named fields, where each field can store a value, such as a number, another object, or a function.

For example, an object might represent a user profile:

```motoko no-repl
  let user = object {
    let name = "Motoko";
    let age = 30;

    public func greet() : Text {
      "Hello, " # name;
    };
  };
```

In this object declaration, `name` and `age` are fields holding values, while `greet` is a field holding a function.

Object fields can have explicit visibility modifiers, such as `public` or `private`. Objects also support async functions and structured behavior.

```motoko no-repl
let motoko = object {
    public let name = "Motoko";
    public var age = 30;

    public func greet() : Text {
        "Hello, my name is " # name # "!"
    };
};

Debug.print(motoko.greet()); // "Hello, my name is Motoko!"
```

### Accessing object fields

Fields and methods of an object are accessed using [dot notation](https://en.wikipedia.org/wiki/Object-oriented_programming).

```motoko no-repl
let motoko = object {
    public let name = "Motoko";
    public var age = 30;

    public func greet() : Text {
        "Hello, my name is " # name # "!"
    };
    public func birthday() : Text{
      age += 1;
      "It's my birthday, I'm now " # debug_show(age) # " years old!"
    }
};
Debug.print(motoko.greet());
Debug.print(motoko.birthday());
```

## Records

Records are typically used to store structured data. Fields are always accessible using [dot notation](https://en.wikipedia.org/wiki/Object-oriented_programming). Records do not support visibility modifiers (`public`, `private`). However, they do support `and` and `with` for combination and modification.

```motoko no-repl
let ghost = {
    var name : Text = "Motoko";
    var age : Nat = 25;
};

Debug.print(ghost.name); // "Motoko"
ghost.age += 1; // Updates age
```

## Comparing records and objects

| Feature | Record | Object |
|---------|--------|--------|
| Named fields | Yes | Yes |
| Private fields | No | Yes |
| Recursively defined fields | No | Yes |
| Mutability | Supports [`let` (immutable) and `var` (mutable)](https://internetcomputer.org/docs/motoko/fundamentals/declarations/variable-declarations) | [Supports `let` (immutable) and `var` (mutable)](https://internetcomputer.org/docs/motoko/fundamentals/declarations/variable-declarations) |
| Supports field visibility (`public`, `private`) | No | Yes |
| Combination (`and`, `with`) | Yes | Yes |
| Supports async behavior | No | Yes (supports `async` functions) |


<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />
