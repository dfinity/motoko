---
sidebar_position: 10
---

# Stable types  

Stable types are essential for preserving state across canister upgrades in Motoko. They allow applications to evolve while ensuring that critical data remains intact. Unlike shared types, which focus on inter-canister communication, stable types ensure data persistence over time.  

A type is stable if it can be safely stored and restored during an upgrade. This includes all shared types, as well as certain mutable structures that are designed for long-term storage.

## Common stable types

### Primitive types

Most primitive types in Motoko are stable:

```motoko norepl
// Numbers, text, and booleans are stable
stable var counter: Nat = 0;
stable var greeting: Text = "Welcome";
stable var isActive: Bool = true;
```

### Immutable and mutable collections  

Both immutable and mutable collections of stable types are stable:  

```motoko norepl
// Immutable arrays are stable
stable var usernames: [Text] = ["Alice", "Bob"];

// Mutable arrays are also stable (unlike shared types)
stable var scores: [var Nat] = [var 100, 85, 92];
```

### Records with mutable or immutable fields  

Records that contain only stable types remain stable, regardless of whether their fields are mutable or immutable.  

```motoko norepl
// Records with immutable fields are stable
stable var config = {
    appName = "MyDApp";
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

Variants are stable when their tags contain only stable types:  

```motoko norepl
// Variants with stable tags are stable
type UserStatus = {
    #online;
    #offline;
    #busy: Text;
};

stable var aliceStatus: UserStatus = #online;
stable var bobStatus: UserStatus = #busy("In a meeting");
```

### Option types  

Option types are stable when they contain stable types:  

```motoko norepl
// Option types with stable inner types are stable
stable var optionalDeadline: ?Nat = ?1640995200000;
stable var optionalMessage: ?Text = null;
```

### Regions  

The `Region` type, which provides low-level memory management, is stable:  

```motoko norepl
// Regions are stable
stable var storage: Region = Region.new();
```

### Actor references  

References to actors are stable, allowing stable canister-to-canister interactions:  

```motoko norepl
// Actor types are stable
type LoggerActor = actor {
    log: shared (message: Text) -> async ();
};

stable var logger: ?LoggerActor = null;
```

## Stable vs shared types

While all shared types are stable, the reverse is not true. Some stable types, such as mutable structures, cannot be shared across canisters.

| Type                            | Stable | Shared |
|---------------------------------|--------|--------|
| Primitive types (`Nat`, `Text`, `Bool`, etc.) | Yes | Yes |
| Immutable arrays (`[T]`)     | Yes | Yes |
| Mutable arrays (`[var T]`)   | Yes | No  |
| Records with immutable fields | Yes | Yes |
| Records with mutable fields   | Yes | No  |
| Option types (`?T`)           | Yes | Yes |
| Variants with shared types    | Yes | Yes |
| Variants with stable types    | Yes | No (if non-shared types are included) |
| Actor references              | Yes | Yes |
| Error type (`Error`)          | No  | No  |

### Mutable arrays

```motoko norepl
// Mutable arrays are stable but NOT shared
stable var counters: [var Nat] = [var 0, 0, 0];
```

### Objects with mutable fields

```motoko norepl
// Objects with mutable fields are stable but NOT shared
stable var user = {
    var name = "Alice";
    var loginCount = 0;
};
```

## How stable variables work  

Declaring a variable as `stable` ensures its persistence across canister upgrades.  

```motoko norepl
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
