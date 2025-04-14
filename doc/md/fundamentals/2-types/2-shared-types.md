---
sidebar_position: 2
---

# Shared types

Shared types enable safe communication between [canisters](https://internetcomputer.org/docs/building-apps/essentials/canisters), [frontends](https://internetcomputer.org/docs/building-apps/frontends/using-an-asset-canister), and users. They define what data can be passed between different systems while ensuring compatibility with [Candid](https://internetcomputer.org/docs/building-apps/interact-with-canisters/candid/candid-concepts), the interface description language of the Internet Computer.

A shared type must be immutable and [Candid](https://internetcomputer.org/docs/building-apps/interact-with-canisters/candid/candid-concepts)-compatible.

## Importance of shared types

Shareability is essential for several reasons:

- Shared types ensure data can be safely serialized and deserialized across network boundaries.
- Restricting sharing to immutable data prevents synchronization issues between canisters.
- Shared types map directly to [Candid](https://internetcomputer.org/docs/building-apps/interact-with-canisters/candid/candid-concepts), enabling interaction between canisters written in different languages.
- Web and mobile frontends communicate with canisters using shared types.

## Common shared types

### Primitive types

Most [primitive types](https://internetcomputer.org/docs/motoko/fundamentals/types/primitive-types) are shared by default.

```motoko no-repl
// Numbers, text, and booleans are shared
let number: Nat = 42;
let message: Text = "Hello IC";
let flag: Bool = true;
```

### Immutable collections

Collections that cannot be modified after creation are shared, including [immutable arrays](https://internetcomputer.org/docs/motoko/fundamentals/types/immutable-arrays) and [tuples](https://internetcomputer.org/docs/motoko/fundamentals/types/tuples).

```motoko no-repl
// Immutable arrays are shared
let names: [Text] = ["Motoko", "Ghost", "Astronaut"];

// Tuples of shared types are shared
let person: (Text, Nat) = ("Motoko", 25);
```

### Records with immutable fields

Objects with immutable fields containing shared types are shared, including [records](https://internetcomputer.org/docs/motoko/fundamentals/types/records).

```motoko no-repl
// Records with immutable fields are shared
let user = {
    id = "usr123";
    name = "Motoko";
    age = 30;
};
```

### Variants with shared type tags

[Variant types](https://internetcomputer.org/docs/motoko/fundamentals/types/variants) are shared when their tags contain shared types.

```motoko no-repl
// Variant types with shared tags are shared
type Result = {
    #ok: Nat;
    #error: Text;
};

let success: Result = #ok(200);
let failure: Result = #error("Operation failed");
```

### Option types

[Option types](https://internetcomputer.org/docs/motoko/fundamentals/types/options-results) are shared when they contain shared types.

```motoko no-repl
// Option types with shared inner types are shared
let maybeGreeting: ?Text = ?"Hello";
let nothing: ?Nat = null;
```

### Actor references

References to [actors](https://internetcomputer.org/docs/motoko/fundamentals/actors-async) are shared, allowing [canisters](https://internetcomputer.org/docs/building-apps/essentials/canisters) to call each other.

```motoko no-repl
// Actor types are shared
type CounterActor = actor {
    increment: shared () -> async Nat;
    getValue: shared query () -> async Nat;
};
```

### Shared functions

[Function types](https://internetcomputer.org/docs/motoko/fundamentals/types/functions) marked as `shared` are sharable.

```motoko no-repl
// Shared function types are shared
type Callback = shared (Nat) -> async ();
```

## Non-shared types

Certain types cannot be shared between [canisters](https://internetcomputer.org/docs/building-apps/essentials/canisters).

### Mutable collections

```motoko no-repl
// Mutable arrays are NOT shared
let mutableArray: [var Nat] = [var 1, 2, 3];
```

### Objects with mutable fields

```motoko no-repl
// Objects with mutable fields are NOT shared
let mutableUser = {
    var name = "Motoko";
    var age = 30;
};
```

### Error types

```motoko no-repl
// The Error type is NOT shared
let err: Error = Error.reject("Something went wrong");
```

### Objects containing non-shared types

```motoko no-repl
// Objects containing non-shared types are NOT shared
let complex = {
    name = "Document";
    contents = [var 1, 2, 3];  // Contains a mutable array
};
```

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />