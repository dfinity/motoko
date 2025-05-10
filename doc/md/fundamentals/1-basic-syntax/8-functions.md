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

A function should specify a return type. If a return type is not declared or otherwise determined from the context, it defaults to the unit `()` return type.

```motoko no-repl
public func exampleFunction(x : Nat) : Nat {
    x;
};
```

:::info Understanding function types

Motoko functions vary by access and behaviour:

The public functions of an actor, used to send messages to the actor, are special sorts of functions called `shared` functions. Shared functions can only be declared within actors and,  unlike ordinary functions their values can be sent to, that is _shared with_, other actors.
Shared functions come in several flavors: `shared` functions that can modify the state of an actor, `shared query` functions that can read the state of an actor without observably changing its state, but not send any further messages, and `shared composite query` functions that are similar to queries with the power to call other queries.
All shared function, unlike ordinary functions, provide access to the identity of their caller, for applications like access control.

[Learn more about functions](https://internetcomputer.org/docs/motoko/fundamentals/types/functions).

:::

For example, we can rewrite the object above as an actor:

``` motoko
actor Digit {
   var value = 0;
   func reset() { value := 0 };
   public shared func inc() : async (){ 
      value += 1;
      if (value == 10) reset(); 
   };
   public shared query func get() : async Nat { value }; 
}
```

The only difference is that the public functions are now declared `shared` and return their result wrapped futures (`async ()` and `async nat`)

The private `reset()` f unction can stay unchanged.

Since the public functions of an actor must be `shared`, you are allowed to omit the `shared` keyword:

``` motoko
actor Digit {
   var value = 0;
   func reset() { value := 0 };
   public func inc() : async () { 
      value += 1;
      if (value == 10) reset(); 
   };
   public query func get() : async Nat { value }; 
}
```

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />