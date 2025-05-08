---
sidebar_position: 8
hide_table_of_contents: true
---

# Functions

Functions in Motoko can take on a variety of different attributes, the most basic of which is if the function is public or private. Public functions are callable by users or other [canisters](https://internetcomputer.org/docs/building-apps/essentials/canisters), while private functions can only be called within the program that defines it.

The most basic Motoko [function declaration](https://internetcomputer.org/docs/motoko/fundamentals/declarations/function-declarations) is:

```motoko no-repl
func exampleFunction() : () {};
```

Within objects, modules and actors all functions are declared private unless explicitly declared `public`.

```motoko no-repl
object Counter  {
   var value = 0;
   func reset() { value := 0 };
   public func inc() { value := 1};
   public func get() : Nat { value }; 
}
```
The object `Counter` has two public methods, the functions `Counter.inc()` and `Counter.get()`. Both `value` and `reset()` are implicitly `private` - attempts to access `Counter.reset()` and `Counter.value` are type errors.
```

A function should specify a return type. If a return type is not declared, it defaults to the unit `()` return type.

```motoko no-repl
public func exampleFunction(x : Nat) : Nat {
    x;
};
```

:::info Understanding function types

Motoko functions vary by access and behaviour:

- `private`: Not exposed outside the namespace(actor, class, module).
- `public`: Makes the function externally callable; can be query or update.
- `shared`: Enables the actor to identify the caller.
- `query`: Reads data without modifying state.
- `composite query`: Reads state, can call other queries.

[Learn more about functions](https://internetcomputer.org/docs/motoko/fundamentals/types/functions).

:::

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />