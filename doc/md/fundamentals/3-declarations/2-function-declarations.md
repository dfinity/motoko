---
sidebar_position: 2
---

# Functions declarations

A function in Motoko is a reusable block of code that takes inputs, processes them, and returns a result. Functions can be named or anonymous, and can optionally specify parameter and return types.

## Declaring a function  

Functions are declared using the `func` keyword. A named function assigns a function to an identifier, allowing recursion and reuse.

```motoko
func add(a: Int, b: Int) : Int {
    return a + b;
}
```

- `add` is the function name.
- `(a: Int, b: Int)` are parameters with types.
- `: Int` specifies the return type.
- `return a + b;` exits the function and provides a result.

## Calling a function  

To execute a function, pass the required arguments:  

```motoko
func add(a: Int, b: Int) : Int {
    return a + b;
};
add(3, 5);
```

## Functions as values  

Functions in Motoko are first-class values, meaning they can be assigned to variables, stored in data structures, and passed as arguments.

```motoko
let double = func (x: Int) : Int { x * 2 };
let applyFunction = func (f: (Int) -> Int, value: Int) : Int { f(value) };

applyFunction(double, 10);
```

`applyFunction` takes a function `f` and applies it to `value`.  `double` is passed as an argument, demonstrating higher-order functions.

## Anonymous functions

Motoko supports functions without names (anonymous). They are often used in expressions or passed as arguments.

```motoko
let multiply = func (x: Int, y: Int) : Int { x * y };
multiply(4, 2);
```

The function is assigned to `multiply` instead of being named directly. `{ x * y }` is the function body without `return`, as single expressions are implicitly returned.


## Recursive functions  

A function that can call itself is known as a recursive function. Recursion allows looping behavior without explicit loops.


```motoko
func factorial(n: Nat) : Nat {
    if (n == 0) {
        return 1;
    };
// Calls itself with factorial(n - 1), reducing n until reaching 0.  
    return n * factorial(n - 1);
}
```

## Shared functions in actors  

In actors, functions can be marked as `shared` to allow asynchronous, inter-canister communication.

```motoko
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
// msg.caller retrieves the Principal of the caller.
    public shared(msg) func whoAmI() : async Principal {
        return msg.caller;
    }
}
```

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />