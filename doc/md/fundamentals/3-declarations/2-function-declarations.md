---
sidebar_position: 2
---

# Functions declarations

A function in Motoko is a reusable block of code that takes inputs, processes them, then returns a result. Functions can be named or anonymous, and can optionally specify parameter and return types.

## Declaring a function

Functions are declared using the `func` keyword. A named function assigns a function to an identifier, allowing recursion and reuse.

```motoko no-repl
// The function is named 'add'
// 'a: Int' and 'b: Int' are parameters with types.
// 'Int' is the return type.
func add(a: Int, b: Int) : Int {
    // Exits the function and provides a result.
    return a + b;
}
```

## Calling a function

To execute a function, pass the required arguments:

```motoko no-repl
func add(a: Int, b: Int) : Int {
    return a + b;
};
add(3, 5);
```

## Functions as values

Functions in Motoko are first-class values. They can be assigned to variables, stored in data structures, and passed as arguments.

```motoko no-repl
let double = func (x: Int) : Int { x * 2 };
let applyFunction = func (f: (Int) -> Int, value: Int) : Int { f(value) };

applyFunction(double, 10);
```

`applyFunction` takes a function `f` and applies it to `value`.  `double` is passed as an argument, demonstrating higher-order functions.

## Anonymous functions

Motoko supports functions without names (anonymous). They can be used in expressions or passed as arguments.

```motoko no-repl
let multiply = func (x: Int, y: Int) : Int { x * y };
multiply(4, 2);
```

The function is assigned to `multiply` instead of being named directly. `{ x * y }` is the function body without `return`, as single expressions are implicitly returned.


## Recursive functions

A function that can call itself is a recursive function. Recursion allows looping behavior without explicit loops.

```motoko no-repl
func factorial(n: Nat) : Nat {
    if (n == 0) {
        return 1;
    };
// Calls itself with factorial(n - 1), reducing n until reaching 0.
    return n * factorial(n - 1);
}
```

## Shared functions in actors

In actors, functions can be marked as `shared` to allow [asynchronous](https://internetcomputer.org/docs/motoko/fundamentals/actors-async#async--await) [inter-canister](https://internetcomputer.org/docs/motoko/fundamentals/messaging) communication.

```motoko no-repl
actor Counter {
    stable var count: Nat = 0;

    public func inc() : async () {
        count += 1;
    };

    public shared query func getCount() : async Nat {
        return count;
    }
};
await Counter.inc();
await Counter.getCount();
```

### Accessing the caller identity

One key advantage of `shared` functions is that they can access the caller's `Principal`, which represents the entity (user or canister) making the request. This allows functions to enforce access control or record who invoked them.

```motoko
actor Example {
// msg.caller retrieves the Principal of the caller.
    public shared(msg) func whoAmI() : async Principal {
        return msg.caller;
    }
}
```

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />