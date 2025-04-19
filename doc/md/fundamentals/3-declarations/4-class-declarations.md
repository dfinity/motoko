---
sidebar_position: 4
---

# Class declarations

A class in Motoko is a blueprint for creating [objects](https://internetcomputer.org/docs/motoko/fundamentals/declarations/object-declaration). It encapsulates [state](https://internetcomputer.org/docs/motoko/fundamentals/state) (fields) and behavior (methods). Instances of a class can be created with specific data. Unlike records and objects, classes support constructors, enabling each instance to have unique values upon creation.

## Defining a class

A class in Motoko defines both:

- A constructor for creating instances.
  - Fields to store data.
  - Methods to perform operations.
- A type describing the instance’s methods.

Generic classes can be used to abstract over types, enabling reusable and type-safe definitions.

```motoko no-repl
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

```motoko no-repl
import Nat "mo:base/Nat";

let natComp = Comparator.Comparator<Nat>(Nat.compare);

assert natComp.lessThan(3, 5);
assert natComp.equal(4, 4);
assert not natComp.greaterThan(1, 2);
```

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />