---
sidebar_position: 4
---

# Class declarations

A class in Motoko serves as a blueprint for creating [objects](https://internetcomputer.org/docs/motoko/fundamentals/declarations/object-declaration) that encapsulate both [state](https://internetcomputer.org/docs/motoko/fundamentals/state) and behavior. It defines fields to hold data and methods to operate on that data. Unlike records and plain objects, classes support constructors, allowing developers to initialize each instance with unique values at creation time.

Classes in Motoko are not the same as classes like in other object oritentated programming languages, but they serve the same purpose. Motoko also doesn’t have a `this` or `self` keyword because you can simply call other methods directly by name.

Use a class when you need to:
- Create multiple objects with the same fields and methods.
- Encapsulate state and behavior inside an object.
- Simulate object orientated programming-like design patterns.

Classes are particularly useful when using multiple objects with similar structures but different initial states. You can think of classes as a template for creating objects. You define fields and methods, and then create new instances by calling the class like a function.
 
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

## Actor classes

Actor classes enable you to create networks of actors programmatically. These classes must be defined in separate source files.

```motoko no-repl title="Counter.mo"
actor class Counter() {
  var count : Int = 0;

  public func increment() : async Int {
    count += 1;
    return count;
  };

  public func get() : async Int {
    return count;
  };
}
```

This defines a `Counter` actor class with:

- A mutable variable `count`.

- A method `increment` that increases `count` and returns the new value.

- A method get that returns the current count.

Then, in another file you can create an instance of this actor class:

```motoko no-repl title="CallCounter.mo"
import Counter "./Counter";

actor {
  let counter = await (with cycles = 1_000_000_000_000) Counter.Counter();

  let newCount = await counter.increment();

  Debug.print("Count is now: " # Int.toText(newCount));

  let currentCount = await counter.get();
  Debug.print("Current count: " # Int.toText(currentCount));
}
```

:::note

On ICP, calls to a class constructor must be provisioned with cycles to pay for the creation of a principal. See [ExperimentalCycles](../base/ExperimentalCycles.md) for instructions on how to add cycles to a call using the imperative `ExperimentalCycles.add<system>(cycles)` function.

:::

### Configuring and managing actor class instances

On ICP, the primary constructor of an imported actor class always creates a new principal and installs a fresh instance of the class as the code for that principal.

To offer more control over actor class installation, Motoko provides each imported actor class with a secondary constructor. This constructor takes an additional first argument that specifies the desired installation mode.

This advanced constructor is only available via special `system` syntax, highlighting its low-level nature.

Using this `system` syntax, developers can:

- Specify initial canister settings (e.g., an array of controllers).
- Manually install, upgrade, or reinstall canisters.
- Access lower-level canister management features provided by ICP.

[Learn more about actor class management](https://internetcomputer.org/docs/motoko/main/reference/language-manual#actor-class-management).

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />
