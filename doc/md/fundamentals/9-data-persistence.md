---
sidebar_position: 9
---

# Data persistence

## Stable declarations

Stable declarations enable data to persist across [canister upgrades](https://internetcomputer.org/docs/building-apps/canister-management/upgrade). This ensures that important [state](https://internetcomputer.org/docs/motoko/fundamentals/state) variables are retained, preventing unintended data loss. Without marking a variable as stable, it is considered **transient** by default, meaning it will be **reset on upgrade**.

The declarations `let` or `var` within an [actor](https://internetcomputer.org/docs/motoko/fundamentals/actors-async) are used as either **stable** or **transient** variables.

1. `stable` declarations:
   - Persist their value across canister upgrades.
   - Are automatically retained as long as they are directly or indirectly reachable from a stable variable.
   - Should be used for core application state, such as counters, user balances, or configuration data.

2. `transient` declarations:
   - Reset to default on every upgrade.
   - Should be used for temporary state or high-order types (e.g., function references) that should not persist.
    <!----Not sure if flexible should be included considering its deprecated but it seems to still exist in the syntax--->

The following example demonstrates a stable counter that retains its value across upgrades:

```motoko no-repl
actor Counter {
  stable var value = 0;

  public func inc() : async Nat {
    value += 1;
    return value;
  };
}
```

### Persistent actors

As of Motoko `v0.13.5`, the recommended way to ensure all variables are stable by default is to use a persistent actor. Declaring an actor as `persistent` means all variables inside it are stable by default. Explicit `stable` declarations are no longer required unless marking an exception as `transient`.

```motoko no-repl
persistent actor Counter {
  var value = 0;  // Implicitly stable!

  public func inc() : async Nat {
    value += 1;
    return value;
  };
}
```

## Stable signatures

Stable variables declared in an actor are represented in a stable signature, which records their names, types, and mutability. This representation captures all stable fields within an actor, providing a structured view of the stable storage layout. The `moc` compiler can generate the stable signature of an actor or actor class into a `.most` file using the `--stable-types` option. `.most` files **should not** be manually created or modified.

```motoko no-repl
actor {
  stable x : Nat;
  stable var y : Int;
  stable z : [var Nat];
};
```

## Stable compatibility

Ensuring compatibility between stable signatures is essential when upgrading an actor. A newer stable signature is considered stable-compatible with an older signature if, for each stable field in the old version, one of the following conditions holds:

- The field no longer exists in the new version (indicating it has been safely removed).
- The field exists in the new version with a supertype of the old type (allowing previously stored values to remain valid).

Consider an actor with the following stable signature in the initial version:

```motoko no-repl
// Version 1
actor {
  stable x : Nat;
  stable var y : Int;
  stable z : [var Nat];
};
```

A compatible upgrade may introduce new fields, remove existing fields, or change types while maintaining compatibility.

```motoko no-repl
// Version 2 (compatible)
actor {
  stable x : Nat;        // Unchanged (compatible)
  stable var y : Int;    // Unchanged (compatible)
  stable z : [var Nat];  // Unchanged (compatible)
  stable newField : Bool; // New fields can be added
};
```

However, an incompatible upgrade would introduce a type change that does not satisfy the subtyping rule.

```motoko no-repl
// Version 2 (incompatible)
actor {
  stable x : Bool;  // Incompatible: Nat is not compatible with Bool
  stable var y : Int;
  stable z : [var Nat];
};
```

In this case, [`Nat`](https://internetcomputer.org/docs/motoko/base/Nat) is not a subtype of [`Bool`](https://internetcomputer.org/docs/motoko/base/Bool), meaning existing values of `x` from version 1 cannot be safely stored in version 2.

### Verifying stable compatibility

To ensure that an upgrade does not break stable storage, the compatibility of two versions can be checked using the `moc` compiler, where `v1.most` contains the stable signature of the older version, and `v2.most` contains the stable signature of the newer version. This verification ensures that existing stable data remains valid after an upgrade.

```sh
moc --stable-compatible v1.most v2.most
```


## Candid compatibility

To ensure canister upgrades do not break existing clients, the Candid interface must remain **backward compatible**. A new version of a canister is Candid-compatible with the previous version if:

- Existing functions retain the same name, parameters, and return types (or expand to more flexible types).
- New functions can be added without affecting existing ones.
- Function signatures do not change in a way that would invalidate existing clients.

Breaking changes, such as renaming functions or altering parameter types without retaining compatibility, may cause clients relying on the old interface to fail.

To illustrate Candid compatibility, consider an initial version of a canister with the following Motoko actor:

```motoko no-repl
actor Counter {
  var value : Nat = 0;

  public func inc() : async Nat {
    value += 1;
    return value;
  };
}
```

This generates the following Candid interface (`.did` file).

```candid no-repl
service Counter : {
  inc : () -> (nat);
}
```

Now, suppose the canister is upgraded and the function `inc` is renamed to `increment`.

```motoko no-repl
actor Counter {
  var value : Nat = 0;

  public func increment() : async Nat {  // Function renamed
    value += 1;
    return value;
  };
}
```

This results in the following updated Candid interface.

```candid no-repl
service Counter : {
  increment : () -> (nat);
}
```

In this case, the upgrade breaks compatibility because the function `inc` is no longer present in the new interface. Any clients expecting to call `inc` will fail since the function is missing.

## Migration

Data representation often changes with a new program version. It is important the program's language allows flexible data migration to the new version.

Motoko supports both implicit migration and explicit migration.

### Implicit migration

Implicit migration occurs **automatically** when the new program version is **stable-compatible** with the old version. The Motoko runtime system handles the migration seamlessly during an upgrade.

The following changes are implicitly migrated:

- Adding or removing actor fields.
- Changing the mutability of a field.
- Removing fields from a [record](https://internetcomputer.org/docs/motoko/fundamentals/types/records) type.
- Adding new fields to a [variant](https://internetcomputer.org/docs/motoko/fundamentals/types/variants) type.
- Changing [`Nat`](https://internetcomputer.org/docs/motoko/base/Nat) to [`Int`](https://internetcomputer.org/docs/motoko/base/Int) (since [`Nat`](https://internetcomputer.org/docs/motoko/base/Nat) is a subtype of [`Int`](https://internetcomputer.org/docs/motoko/base/Int).
- Adjustments in shared function parameters and return types.
- Any transformation allowed by Motoko’s subtyping rules.

If a change falls within these categories, no manual migration is required. The system ensures that stable variables persist correctly across upgrades.

### Explicit migration

For structural changes that **are not stable-compatible**, an explicit migration must be performed. This is necessary when changing a stable variable’s type in an incompatible way, such as converting [`Int`](https://internetcomputer.org/docs/motoko/base/Int) to [`Float`](https://internetcomputer.org/docs/motoko/base/Float).

Explicit migration follows a three-step approach:

1. Introduce new stable variables while keeping the old ones.
2. Transfer data from the old variables to the new ones during the upgrade.
3. Remove the old variables once all data has been migrated.

Attempting to change `state: Int` directly to [`Float`](https://internetcomputer.org/docs/motoko/base/Float) is incompatible, so a new variable `newState` is introduced:

```motoko no-repl
import Debug "mo:base/Debug";
import Float "mo:base/Float";

persistent actor Counter_v1 {
  var state : Int = 0; // Old variable (implicitly stable)
  var newState : Float = Float.fromInt(state); // New stable variable

  public func increment() : async () {
    newState += 0.5;
  };

  public func decrement() : async () {
    newState -= 0.5;
  };

  public query func read() : async Int {
    Debug.trap("No longer supported: Use `readFloat`");
  };

  public query func readFloat() : async Float {
    return newState;
  };
};
```

In this version:

- The `newState` variable stores the floating-point value.
- The old `state` variable remains to facilitate migration.
- The `read` function is retained but now raises a trap, guiding users to use `readFloat`.

Once all data has been migrated, the old `state` variable can be removed:

```motoko no-repl
import Debug "mo:base/Debug";

persistent actor Counter_v2 {
  var newState : Float = 0.0; // Stable variable after migration

  public func increment() : async () {
    newState += 0.5;
  };

  public func decrement() : async () {
    newState -= 0.5;
  };

  public query func readFloat() : async Float {
    return newState;
  };
};
```

Alternatively, to retain compatibility while indicating that a variable is no longer in use, the type of `state` can be changed to `Any`. It can hold any value but requires explicit downcasting to retrieve data.

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />