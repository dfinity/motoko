---
sidebar_position: 2
---

# Functions declarations

A function in Motoko is a reusable block of code that accepts inputs, performs computations or actions, and optionally returns a result. Functions can be either named or anonymous and may explicitly define the types of their parameters and return values for clarity and type safety.

## Declaring a function

Functions are declared using the `func` keyword. A named function assigns the function to an identifier, making it accessible for reuse and enabling recursion. This allows the function to refer to itself by name within its own body. In contrast, anonymous functions (e.g., lambda functions or function expressions) do not have a name and are typically defined inline. These functions are often assigned to variables or passed directly as arguments to other functions.

```motoko no-repl
// The function is named 'add'
// 'a : Int' and 'b : Int' are parameters with types.
// 'Int' is the return type.
func add(a : Int, b : Int) : Int {
    // Exits the function and provides a result.
    return a + b;
}
```

## Calling a function

To execute a function, you simply call it by its name (or reference, in the case of anonymous functions) and pass the required arguments in parentheses. The arguments must match the number and types specified in the function’s definition.

```motoko no-repl
func add(a : Int, b : Int) : Int {
    return a + b;
};
add(3, 5);
```

## Functions as values

Functions in Motoko are first-class values, meaning they can be treated like any other value in the language. They can be assigned to variables, stored in data structures such as arrays or records, and passed as arguments to other functions.

```motoko no-repl
func double(x : Int) : Int { x * 2 };
func applyFunction(f : Int -> Int, value : Int) : Int { f(value) };

applyFunction(double, 10);
```

`applyFunction` takes a function `f` and applies it to `value`.  `double` is passed as an parameter, allowing to construct higher-order functions.

## Anonymous functions

Motoko supports functions without names (anonymous). They can be used in expressions or passed as arguments.


``` motoko no-repl
func applyFunction(f :  Int -> Int, value : Int) : Int { f(value) };
applyFunction( func (x : Int) : Int { x * 2 } , 2);
```

In this example, the first argument to `applyFunction` is the anonymous function `func (x : Int) : Int { x * 2 }`.
This is just an anonymous version of the function named `double` above.

The compiler can infer the argument and result types of anonymous functions, when the types are determined from the context, so you can even just write:

``` motoko no-repl
applyFunction( func x { x * 2 } , 2);
```

## Recursive functions

A function that can call itself is a recursive function. Recursion enables looping behavior by having a function repeatedly invoke itself, typically with modified arguments, until a base case is reached. However, each recursive call consumes stack space by allocating a new stack frame, which uses memory on the program's relatively small call stack. As a result, excessive or uncontrolled recursion can lead to stack overflow or out-of-memory errors.

```motoko no-repl
func factorial(n : Nat) : Nat {
    if (n == 0) {
        return 1;
    };
// Calls itself with factorial(n - 1), reducing n until reaching 0.
    n * factorial(n - 1);
}
```
::: warning

In a recursive function, each recursive call consumes stack space by allocating a new stack frame, which uses memory on the program's relatively small call stack. As a result, excessive or uncontrolled recursion can lead to stack overflow or out-of-memory errors.

:::

## Generic functions

Generic functions are used to write reusable logic that works with any type.

For example, the following function applies a given function twice to a value:

```motoko no-repl
func twice<T>(f : T -> T) : T -> T = func (x : T) {
  f(f(x))
};
```

You can use it with different types:

```motoko no-repl
let double = twice<Nat>(func x { x * 2 });
assert double(2) == 8;

let echoTwice = twice<Text>(func s { s # "!" });
assert echoTwice("Hello") == "Hello!!";
```

## Shared functions in actors

In actors, functions can be marked as `shared` to allow [asynchronous](https://internetcomputer.org/docs/motoko/fundamentals/actors-async#async--await) [inter-canister](https://internetcomputer.org/docs/motoko/fundamentals/messaging) communication.

```motoko no-repl
actor Counter {
    stable var count : Nat = 0;

    public func inc() : async () {
        count += 1;
    };

    public shared query func getCount() : async Nat {
        count;
    }
};
await Counter.inc();
await Counter.getCount();
```

### Accessing the caller identity

One key advantage of shared functions in Motoko is that they have access to the caller's Principal, which uniquely identifies the entity (user or another canister) that made the request. This capability allows actors to implement access control by verifying the caller’s identity before performing sensitive operations.

```motoko
actor Example {
    // msg.caller retrieves the Principal of the caller.
    public shared(msg) func whoAmI() : async Principal {
        return msg.caller;
    }
}
```

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />