---
sidebar_position: 4
---

# Class declarations

A class in Motoko is a blueprint for creating [objects](https://internetcomputer.org/docs/motoko/fundamentals/declarations/object-declaration). It encapsulates [state](https://internetcomputer.org/docs/motoko/fundamentals/state) (fields) and behavior (methods). Instances of a class can be created with specific data. Unlike records and objects, classes support constructors, enabling each instance to have unique values upon creation.

## Defining a class

A class is declared using the `class` keyword. It includes:

- Fields to store data.
- Methods to perform operations.
- A constructor to initialize instance values.

```motoko no-repl
// Define parameters that initialize the instance.
class Ghost(name: Text, age: Nat) {

    // Returns a message
    public func greet() : Text {
        "Hello, my name is " # name # " and I am " # Nat.toText(age) # " years old."
    };
};
```

## Creating instances

To create an instance of a class, call the **constructor** using the class name.

```motoko no-repl
class Ghost(name: Text, age: Nat) {
    public func greet() : Text {
        "Hello, my name is " # name # " and I am " # Nat.toText(age) # " years old."
    };
};

// Creates a new instance of Ghost
let motoko = Ghost("Motoko", 30);

// Called on the instance motoko
Debug.print(motoko.greet());
```

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />