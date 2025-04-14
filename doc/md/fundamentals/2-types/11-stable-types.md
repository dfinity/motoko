---
sidebar_position: 11
---

# Stable types

Stable types are essential for preserving [state](https://internetcomputer.org/docs/motoko/fundamentals/state) across [canister upgrades](https://internetcomputer.org/docs/building-apps/canister-management/upgrade). Unlike shared types, which focus on [inter-canister](https://internetcomputer.org/docs/motoko/fundamentals/messaging) communication, stable types ensure data persistence over time.

A type is stable if it can be safely stored and restored before and after an upgrade. This includes all shared types, as well as certain mutable structures that are designed for long-term storage.

## Stable vs shared types

While all shared types are stable, the reverse is not true. Some stable types cannot be shared across [canisters](https://internetcomputer.org/docs/building-apps/essentials/canisters).

| Type                            | Stable | Shared |
|---------------------------------|--------|--------|
| Primitive types ([`Nat`](https://internetcomputer.org/docs/motoko/base/Nat), [`Text`](https://internetcomputer.org/docs/motoko/base/Text), [`Bool`](https://internetcomputer.org/docs/motoko/base/Bool), etc.) | Yes | Yes |
| Immutable arrays (`[T]`)     | Yes | Yes |
| Mutable arrays (`[var T]`)   | Yes | No  |
| Records with immutable fields | Yes | Yes |
| Records with mutable fields   | Yes | No  |
| Option types (`?T`)           | Yes | Yes |
| Variants with shared types    | Yes | Yes |
| Variants with stable types    | Yes | No (if non-shared types are included) |
| Actor references              | Yes | Yes |
| Error type (`Error`)          | No  | No  |

## Common stable types

### Primitive types

Most [primitive types](https://internetcomputer.org/docs/motoko/fundamentals/types/primitive-types) in Motoko are stable.

```motoko no-repl
// Numbers, text, and booleans are stable
stable var counter: Nat = 0;
stable var greeting: Text = "Welcome";
stable var isActive: Bool = true;
```

### Immutable and mutable collections

Both immutable and mutable collections of stable types are stable.

```motoko no-repl
// Immutable arrays are stable
stable var usernames: [Text] = ["Motoko", "Ghost"];

// Mutable arrays are also stable (unlike shared types)
stable var scores: [var Nat] = [var 100, 85, 92];
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
    #busy: Text;
};

stable var motokoStatus: UserStatus = #online;
stable var ghostStatus: UserStatus = #busy("In a meeting");
```

### Option types

[Option](https://internetcomputer.org/docs/motoko/fundamentals/types/options-results) types are stable when they contain stable types.

```motoko no-repl
// Option types with stable inner types are stable
stable var optionalDeadline: ?Nat = ?1640995200000;
stable var optionalMessage: ?Text = null;
```

### Regions

The [`Region`](https://internetcomputer.org/docs/motoko/base/Region) type, which provides low-level memory management, is stable.

```motoko no-repl
// Regions are stable
stable var storage: Region = Region.new();
```

### Actor references

References to [actors](https://internetcomputer.org/docs/motoko/fundamentals/actors-async) are stable, allowing stable canister-to-canister interactions.

```motoko no-repl
// Actor types are stable
type LoggerActor = actor {
    log: shared (message: Text) -> async ();
};

stable var logger: ?LoggerActor = null;
```

### Mutable arrays

Mutable arrays are stable but not shared.

```motoko no-repl
stable var counters: [var Nat] = [var 0, 0, 0];
```

### Objects with mutable fields

Objects with mutable fields are stable but not shared.

```motoko no-repl
stable var user = {
    var name = "Motoko";
    var loginCount = 0;
};
```

## How stable variables work

Declaring a variable as `stable` ensures its persistence across canister upgrades.

```motoko no-repl
actor Counter {
    // This value persists across upgrades
    stable var count: Nat = 0;

    // This value resets after each upgrade
    var tempCount: Nat = 0;

    public func increment(): async Nat {
        count += 1;
        tempCount += 1;
        return count;
    };

    public query func getCount(): async Nat {
        return count;
    };

    public query func getTempCount(): async Nat {
        return tempCount;
    };
}
```

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />