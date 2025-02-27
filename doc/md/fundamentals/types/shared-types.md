---
sidebar_position: 9
---

# Shared types

Shared types enable safe communication between canisters, frontends, and users. They define what data can be passed between different systems while ensuring compatibility with Candid, the interface description language of the Internet Computer.

A type must be immutable and Candid-compatible to be shared.

## Common shared types

### Primitive types

Most primitive types are shared by default:

```motoko no-repl
// Numbers, text, and booleans are shared
let number: Nat = 42;
let message: Text = "Hello IC";
let flag: Bool = true;
```

### Immutable collections

Collections that cannot be modified after creation are shared:

```motoko no-repl
// Immutable arrays are shared
let names: [Text] = ["Alice", "Bob", "Charlie"];

// Tuples of shared types are shared
let person: (Text, Nat) = ("Alice", 25);
```

### Records with immutable fields

Objects with immutable fields containing shared types are shared:

```motoko norepl
// Records with immutable fields are shared
let user = {
    id = "usr123";
    name = "Alice";
    age = 30;
};
```

### Variants with shared type tags

Variant types are shared when their tags contain shared types:

```motoko norepl
// Variant types with shared tags are shared
type Result = {
    #ok: Nat;
    #error: Text;
};

let success: Result = #ok(200);
let failure: Result = #error("Operation failed");
```

### Option types

Option types are shared when they contain shared types:

```motoko no-repl
// Option types with shared inner types are shared
let maybeGreeting: ?Text = ?"Hello";
let nothing: ?Nat = null;
```

### Actor references

References to actors are shared, allowing canisters to call each other:

```motoko no-repl
// Actor types are shared
type CounterActor = actor {
    increment: shared () -> async Nat;
    getValue: shared query () -> async Nat;
};
```

### Shared functions

Function types marked as `shared` are sharable:

```motoko no-repl
// Shared function types are shared
type Callback = shared (Nat) -> async ();
```

## Non-shared types

Certain types cannot cross canister boundaries:

### Mutable collections

```motoko no-repl
// Mutable arrays are NOT shared
let mutableArray: [var Nat] = [var 1, 2, 3];
```

### Objects with mutable fields

```motoko no-repl
// Objects with mutable fields are NOT shared
let mutableUser = {
    var name = "Alice";
    var age = 30;
};
```

### Error type

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

## Importantance of shared types

Shareability is essential for several reasons:

- Shared types ensure data can be safely serialized and deserialized across network boundaries.
- Restricting sharing to immutable data prevents synchronization issues between canisters.
- Shared types map directly to Candid, enabling interaction between canisters written in different languages.
- Web and mobile frontends communicate with canisters using shared types.
