---
sidebar_position: 3
---

# Object declarations

An object is a collection of named fields that can hold values or [functions](https://internetcomputer.org/docs/motoko/fundamentals/declarations/function-declarations).

## Comparing records and objects

| Feature | Record | Object |
|---------|--------|--------|
| Stores data | Yes | Yes |
| Stores functions | No | Yes |
| Mutability | Supports [`let` (immutable) and `var` (mutable)](https://internetcomputer.org/docs/motoko/fundamentals/declarations/variable-declarations) | [Supports `let` (immutable) and `var` (mutable)](https://internetcomputer.org/docs/motoko/fundamentals/declarations/variable-declarations) |
| Supports field visibility (`public`, `private`) | No | Yes |
| Combination (`and`, `with`) | Yes | Yes |
| Supports async behavior | No | Yes (supports `async` functions) |

### Record example

Records only store data; they do not contain functions. Fields are always accessible using [dot notation](https://en.wikipedia.org/wiki/Object-oriented_programming). Records do not support visibility modifiers (`public`, `private`). However, they do support `and` and `with` for combination and modification.

```motoko no-repl
let ghost = {
    var name : Text = "Motoko";
    var age : Nat = 25;
};

Debug.print(ghost.name); // "Motoko"
ghost.age += 1; // Updates age
```

### Object example

Objects extend records by supporting functions. Fields can have explicit visibility modifiers, such as `public` or `private`. Objects support async functions and structured behavior.

```motoko no-repl
let motoko = object {
    public let name = "Motoko";
    public var age = 30;

    public func greet() : async Text {
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
    public func birthday(): Text{
      age += 1;
      "It's my birthday, I'm now " # debug_show(age) # "years old!"
    }
};
Debug.print(motoko.greet());
Debug.print(motoko.birthday());
```

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />