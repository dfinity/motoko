---
sidebar_position: 1
---
# Defining an actor

In Motoko, an actor is a unit of computation that encapsulates state and behavior. Unlike traditional functions or objects in other programming languages, actors operate independently and interact with each other through asynchronous messaging.

A Motoko program typically starts by defining an actor. The example below declares an actor named `Main` with a `hello` function that returns a `"Hello, world!"` message:

```motoko
actor Main {
  public query func hello() : async Text {
    "Hello, world!"
  };
};

await Main.hello();
```

A Motoko actor always presents its interface as a suite of named functions with defined argument and return types. When Motoko code is compiled, this interface is automatically generated in **[Candid](https://internetcomputer.org/docs/current/developer-docs/smart-contracts/candid/candid-concepts)**, an interface description language.

Since actors in Motoko communicate asynchronously, `await` ensures the result is retrieved once the function completes.

## References

- [Actors](https://internetcomputer.org/docs/current/motoko/main/writing-motoko/actors-async)
