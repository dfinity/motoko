---
sidebar_position: 13
---

# Stable types

Recall that [shared types](https://internetcomputer.org/docs/motoko/fundamentals/types/shared-types) can be transmitted to other actors and canisters as the arguments or results of shared functions.

**Stable types** comprise all shared types and represent the types of values that may be stored in the stable declarations of a Motoko actor.
Storing a value in a stable declaration ensures that the value persists across actor upgrades, enabling state preservation without relying on a file system or database

The ability to retain data across upgrades is a key feature of Motoko, distinguishing it from other mainstream languages.
The set of stable types corresponds to the types of values that can be transferred from an actor to its future upgraded versions.
Types that cannot be transferred include those whose values depend on the actor’s current code, such as non-shared functions or, more generally, objects with function members.

:::info  
In actors declared without the `persistent` keyword, all private declarations are treated as transient unless explicitly marked `stable`.  
In a `persistent` actor, all private declarations—except for function declarations—are considered stable by default, unless explicitly marked `transient`.  
The type of any stable variable must belong to the set of stable types.  
Transient variables are exempt from this restriction and may have any type, including non-stable types.  
:::

To give the user more flexibility, the set of stable types is larger than the set of shared types and includes mutable types. This means that programmers can store their application state using a wide range of imperative (stateful) as well as functional (stateless) data structures. 

## Stable vs shared types

While all shared types are stable, the reverse is not true. Some stable types cannot be shared across [canisters](https://internetcomputer.org/docs/building-apps/essentials/canisters).

| Type                                                                                         | Stable | Shared |
|----------------------------------------------------------------------------------------------|--------|--------|
| Primitive types ([`Nat`](https://internetcomputer.org/docs/motoko/base/Nat), [`Text`](https://internetcomputer.org/docs/motoko/base/Text), [`Bool`](https://internetcomputer.org/docs/motoko/base/Bool), etc.) | Yes    | Yes    |
| Immutable arrays (`[T]`)                                                                     | Yes*   | Yes**  |
| Mutable arrays (`[var T]`)                                                                   | Yes*   | No     |
| Records with immutable fields                                                                | Yes    | Yes    |
| Records with mutable fields                                                                  | Yes    | No     |
| Option types (`?T`)                                                                          | Yes*   | Yes**  |
| Variants with stable types                                                                   | Yes    | No (if non-shared types are included) |
| Shared functions                                                                             | Yes    | Yes    |
| Actor references                                                                             | Yes    | Yes    |
| Regions                                                                                      | Yes    | No     |
| Error type (`Error`)                                                                         | No     | No     |
| Non-shared functions and futures (`async* T`)                                                | No     | No     |

\* provided `T` is stable  
\** provided `T` is shared  

:::info
In types such as `[T]`, `[var T]`, and `?T`, the element type `T` must itself be stable (for stable use) or shared (for shared use).  
Non-shared functions and futures (`async* T`) depend on the current actor’s execution context and cannot be preserved or transferred.  
:::

## Common stable types

### Primitive types

Most [primitive types](https://internetcomputer.org/docs/motoko/fundamentals/types/primitive-types) in Motoko are stable.

```motoko no-repl
// Numbers, text, and booleans are stable
stable var counter : Nat = 0;
stable var greeting : Text = "Welcome";
stable var isActive : Bool = true;
```

### Immutable and mutable collections

Both immutable and mutable collections of stable types are stable.

```motoko no-repl
// Immutable arrays are stable
stable var usernames : [Text] = ["Motoko", "Ghost"];

// Mutable arrays are also stable (unlike shared types)
stable var scores : [var Nat] = [var 100, 85, 92];
```

### Records with mutable or immutable fields

[Records](https://internetcomputer.org/docs/motoko/fundamentals/types/records) that contain only stable types remain stable, regardless of whether their fields are mutable or immutable.

```motoko no-repl
// Records with immutable fields are stable
stable var config = {
    appName = "My_Motoko_App";
    version = "1.0.0";
};

// Records with mutable fields are also stable
stable var settings = {
    var darkMode = false;
    var notifications = true;
    var port = 80;
};
```

### Variants with stable type tags

[Variants](https://internetcomputer.org/docs/motoko/fundamentals/types/variants) are stable when their tags contain only stable types.

```motoko no-repl
// Variants with stable tags are stable
type UserStatus = {
    #online;
    #offline;
    #busy : Text;
};

stable var motokoStatus : UserStatus = #online;
stable var ghostStatus : UserStatus = #busy("In a meeting");
```

### Option types

[Option](https://internetcomputer.org/docs/motoko/fundamentals/types/options-results) types are stable when they contain stable types.

```motoko no-repl
// Option types with stable inner types are stable
stable var optionalDeadline : ?Nat = ?1640995200000;
stable var optionalMessage : ?Text = null;
```

### Regions

The [`Region`](https://internetcomputer.org/docs/motoko/base/Region) type, which provides low-level memory management, is stable.

```motoko no-repl
// Regions are stable
stable var storage : Region = Region.new();
```

### Actor references

References to [actors](https://internetcomputer.org/docs/motoko/fundamentals/actors-async) are stable, allowing stable canister-to-canister interactions.

```motoko no-repl
// Actor types are stable
type LoggerActor = actor {
    log : shared (message : Text) -> async ();
};

stable var logger : ?LoggerActor = null;
```

### Mutable arrays

Mutable arrays are stable but not shared.

```motoko no-repl
stable var counters : [var Nat] = [var 0, 0, 0];
```

### Objects with mutable fields

Objects with mutable fields are stable but not shared.

```motoko no-repl
stable var user = {
    var name = "Motoko";
    var loginCount = 0;
};
```

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />