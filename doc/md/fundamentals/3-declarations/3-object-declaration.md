---
sidebar_position: 3
---

# Object declarations

An object is a collection of named fields that can hold values or [functions](https://internetcomputer.org/docs/motoko/fundamentals/declarations/function-declarations).

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

### Record example

Records are typically used to store structured data. Fields are always accessible using [dot notation](https://en.wikipedia.org/wiki/Object-oriented_programming). Records do not support visibility modifiers (`public`, `private`). However, they do support `and` and `with` for combination and modification.

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
import List "mo:base/List";
import Debug "mo:base/Debug";

let logger = object {
    private var logs = List.empty<Text>()
    
    public func add(message : Text) : () {
        List.add(logs, message);
        Debug.print("New entry: " # message);
    };
    
    public func clear() : () {
        logs.clear();
        Debug.print("Logs cleared");
    };
    
    public func size() : Nat {
        logs.size();
    };
};
```

### Accessing object fields

Fields and methods of an object are accessed using [dot notation](https://en.wikipedia.org/wiki/Object-oriented_programming).

```motoko no-repl
import List "mo:base/List";
import Debug "mo:base/Debug";

let logger = object {
    private var logs = List.empty<Text>()
    
    public func add(message : Text) : () {
        List.add(logs, message);
        Debug.print("New entry: " # message);
    };
    
    public func clear() : () {
        List.clear(logs);
        Debug.print("Logs cleared");
    };
    
    public func size() : Nat {
        logs.size();
    };
};
logger.add("System initialized");
logger.add("User logged in");
Debug.print("Number of logs: " # Nat.toText(logger.size()));
logger.clear();
```

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />