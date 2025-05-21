---
sidebar_position: 4
---

# Class declarations

A class in Motoko serves as a blueprint for creating [objects](https://internetcomputer.org/docs/motoko/fundamentals/declarations/object-declaration) that encapsulate both [state](https://internetcomputer.org/docs/motoko/fundamentals/state) and behavior. It defines fields to hold data and methods to operate on that data. Unlike records and plain objects, classes support constructors, allowing developers to initialize each instance with unique values at creation time. Classes are particularly useful when using multiple objects with similar structures but different initial states.

## Defining a class

A class in Motoko defines both:

- A constructor for creating instances, which includes:
  - Fields to store data.
  - Methods to perform operations on that data.
- A type that describes the interface of the instance, including its available methods.

Motoko also supports generic classes, which allow abstraction over types. This enables the creation of reusable, type-safe components that can work with a variety of data types while maintaining strong compile-time guarantees.

```motoko no-repl
import O "mo:base/Order"; // Required to reference O.Order and its variants

  // A generic comparator class for ordering elements
  class Comparator<T>(compare : (T, T) -> O.Order) {
    public func lessThan(a : T, b : T) : Bool {
      compare(a, b) == #less;
    };

    public func equal(a : T, b : T) : Bool {
      compare(a, b) == #equal;
    };

    public func greaterThan(a : T, b : T) : Bool {
      compare(a, b) == #greater;
    };
  };

```

This declaration creates:

- A constructor: `Comparator : ((K, K) -> Order) -> Comparator<K>`.
- A generic type: `Comparator<K>` that provides comparison methods.

## Creating instances

Creating an instance is when a class constructor is used to produce a new object with its own state and behavior based on the class definition.

```motoko no-repl
class Counter(init : Nat) {
  var count : Nat = init;

  public func inc() : Nat {
    count += 1;
    count
  };
};

// Creating an instance of the class
let myCounter = Counter(0);

// Using the instance
myCounter.inc(); // returns 1
```

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />
