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
//db.mo
import Map "mo:base/OrderedMap";
import Text "mo:base/Text";
actor Database {

  type User = {
    username: Text;
    email: Text;
  };

  transient let userMap = Map.Make<Text>(Text.compare);
  stable var users = userMap.empty<User>();

  public func addUser(id: Text, profile: User) : async () {
    users := userMap.put(users, id, profile);
  };

  public query func getUser(id: Text) : async ?User{
    userMap.get(users, id)
  };
}
```

### Persistent actors

As of Motoko `v0.13.5`, the recommended way to ensure all variables are stable by default is to use a persistent actor. Declaring an actor as `persistent` means all variables inside it are stable by default. Explicit `stable` declarations are no longer required unless marking an exception as `transient`.

```motoko no-repl
import Map "mo:base/OrderedMap";
import Text "mo:base/Text";
persistent actor Database {

  type User = {
    username: Text;
    email: Text;
  };

  transient let userMap = Map.Make<Text>(Text.compare);
  var users = userMap.empty<User>(); //implicitly stable!

  public func addUser(id: Text, profile: User) : async () {
    users := userMap.put(users, id, profile);
  };

  public query func getUser(id: Text) : async ?User{
    userMap.get(users, id)
  };
}
```

## Stable signatures

Stable variables declared in an actor are represented in a stable signature, which records their names, types, and mutability. This representation captures all stable fields within an actor, providing a structured view of the stable storage layout. The `moc` compiler can generate the stable signature of an actor or actor class into a `.most` file using the `--stable-types` option. `.most` files **should not** be manually created or modified.

<!-- Needs revision this is an assumption--->
```motoko no-repl
actor {
  stable var users : Map.Map<Text, User>;
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
  type User = { username: Text; email: Text };
  stable var users : Map.Map<Text, User>;
};
```

A compatible upgrade may introduce new fields, remove existing fields, or change types while maintaining compatibility.

```motoko no-repl
// Version 2 (compatible)
actor {
  type User = { username: Text; email: Text; active: ?Bool };
  stable var users : Map.Map<Text, User>;  // Compatible: added optional field
  stable var settings : Map.Map<Text, Text>;     // New field added
};
```

However, an incompatible upgrade would introduce a type change that does not satisfy the subtyping rule.

```motoko no-repl
// Version 2 (incompatible)
actor {
  type User = { username: Text; email: Text; active: Bool };
  stable var users : Map.Map<Text, User>;  // Incompatible: required field added
};
```

In this case, the `User` type in version 2 requires an active field of type `Bool`, which is not compatible with the
`?Bool` field in version 1 records.

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

To illustrate Candid compatibility, consider an initial version of a database canister with the following Motoko actor:

```motoko no-repl
persistent actor Database {
  import Map "mo:base/OrderedMap";
  import Text "mo:base/Text";

  type User = {
    username: Text;
    email: Text;
  };

  transient let userMap = Map.Make<Text>(textCompare);
  var users = userMap.empty<User>();

  public func addUser(id: Text, user: User) : async () {
    users := userMap.put(users, id, user);
  };

  public query func getUser(id: Text) : async ?User {
    userMap.get(users, id)
  };
}
```

This generates the following Candid interface (`.did` file).

```candid no-repl
service Database : {
  addUser : (text, record { username: text; email: text }) -> ();
  getUser : (text) -> (opt record { username: text; email: text }) query;
}
```

Now, suppose the canister is upgraded and the function `addUser` is renamed to `createUser`.

```motoko no-repl
persistent actor Database {
  import Map "mo:base/OrderedMap";
  import Text "mo:base/Text";

  type User = {
    username: Text;
    email: Text;
  };

  let userMap = Map.Make<Text>(Text.compare);
  var users = userMap.empty<User>();

  public func createUser(id: Text, user: User) : async () {  // Function renamed
    users := userMap.put(users, id, user);
  };

  public query func getUser(id: Text) : async ?User {
    userMap.get(users, id)
  };
}
```

This results in the following updated Candid interface.

```candid no-repl
service Database : {
  createUser : (text, record { username: text; email: text }) -> ();
  getUser : (text) -> (opt record { username: text; email: text }) query;
}
```

In this case, the upgrade breaks compatibility because the function `addUser` is no longer present in the new interface. Any clients expecting to call `addUser` will fail since the function is missing.

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
- Changing [`Nat`](https://internetcomputer.org/docs/motoko/base/Nat) to [`Int`](https://internetcomputer.org/docs/motoko/base/Int) (since [`Nat`](https://internetcomputer.org/docs/motoko/base/Nat) is a subtype of [`Int`](https://internetcomputer.org/docs/motoko/base/Int)).
- Adjustments in shared function parameters and return types.
- Any transformation allowed by Motoko’s subtyping rules.

If a change falls within these categories, no manual migration is required. The system ensures that stable variables persist correctly across upgrades.

### Explicit migration

For structural changes that **are not stable-compatible**, an explicit migration must be performed. This is necessary when changing a stable variable’s type in an incompatible way.

Explicit migration follows a three-step approach:

1. Introduce new stable variables while keeping the old ones.
2. Transfer data from the old variables to the new ones during the upgrade.
3. Remove the old variables once all data has been migrated.

For example, migrating from a simple user database to an enhanced version:

```motoko no-repl
import Debug "mo:base/Debug";
import Map "mo:base/OrderedMap";
import Text "mo:base/Text";

persistent actor Database_v1 {
  type User = {
    username: Text;
    email: Text;
  };

  // New schema with additional fields
  type EnhancedUser = {
    username: Text;
    email: Text;
    verified: Bool;
  };

  // Old schema
  transient let userMap = Map.Make<Text>(Text.compare);
  var users = userMap.empty<User>();

  transient let enhancedUserMap = Map.Make<Text>(Text.compare);
  var enhancedUsers = enhancedUserMap.empty<EnhancedUser>();

  // Migrate data on startup
  // For each existing user record, create an enhanced version
  for ((id, user) in userMap.entries(users)) {
    enhancedUsers := enhancedUserMap.put(
      enhancedUsers, 
      id, 
      {
        username = user.username;
        email = user.email;
        verified = false;
      }
    );
  };

  // Original API maintained for backward compatibility
  public func addUser(id: Text, user: User) : async () {
    users := userMap.put(users, id, user);

    // Also update enhanced database
    enhancedUsers := enhancedUserMap.put(
      enhancedUsers,
      id,
      {
        username = user.username;
        email = user.email;
        verified = false;
      }
    );
  };
  
  public query func getUser(id: Text) : async ?User {
    Debug.print("Warning: getUser is deprecated. Use getEnhancedUser instead.");
    userMap.get(users, id)
  };
  
  // New API for the enhanced schema
  public func addEnhancedUser(id: Text, user: EnhancedUser) : async () {
    enhancedUsers := enhancedUserMap.put(enhancedUsers, id, user);
    
    // Keep old database in sync
    users := userMap.put(
      users,
      id,
      {
        username = user.username;
        email = user.email;
      }
    );
  };
  
  public query func getEnhancedUser(id: Text) : async ?EnhancedUser {
    enhancedUserMap.get(enhancedUsers, id)
  };
};
```

In this version:

- The `enhancedUsers` variable stores the enhanced user records with the additional `verified` field.
- The old `users` variable remains to facilitate migration.
- The `getUser` function is retained but now prints a warning message, guiding users to use `getEnhancedUser` instead.

Once all data has been migrated, the old `Users` schema can be removed:

```motoko no-repl
import Map "mo:base/OrderedMap";
import Text "mo:base/Text";

persistent actor Database_v2 {
  // Only enhanced user schema remains
  type EnhancedUser = {
    username: Text;
    email: Text;
    verified: Bool;
  };
  
  transient let userMap = Map.Make<Text>(Text.compare);
  var users = userMap.empty<EnhancedUser>();
  
  // Legacy API now converts to the new structure
  public func addUser(id: Text, username: Text, email: Text) : async () {
    users := userMap.put(
      users,
      id,
      {
        username;
        email;
        verified = false;
      }
    );
  };
  
  // Enhanced API becomes the primary interface
  public func addEnhancedUser(id: Text, user: EnhancedUser) : async () {
    users := userMap.put(users, id, user);
  };
  
  public query func getEnhancedUser(id: Text) : async ?EnhancedUser {
    userMap.get(users, id)
  };
};
```

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />