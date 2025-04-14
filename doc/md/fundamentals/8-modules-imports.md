---
sidebar_position: 8
---

# Modules and imports

Motoko minimizes built-in types and operations, relying on a base library of modules to provide essential functionality. This modular approach allows the language to evolve while maintaining simplicity.

:::caution

The base library is actively maintained and updates may introduce breaking changes. Developers should review the latest Motoko migration guide when updating dependencies.

:::

## Importing from the base library

The Motoko base library includes common utilities for working with data structures, debugging, and other functionality. To import from the base library, use the `import` keyword, followed by the `mo:base/` module path.

```motoko no-repl
import Debug "mo:base/Debug";

Debug.print("Hello, world!");
```

The `mo:` prefix identifies a Motoko module. The declaration does not include the `.mo` file extension.

## Importing specific functions

Instead of importing an entire module, individual functions can be imported.

```motoko no-repl
import { equal } "mo:base/Nat";

let result = equal(10, 10); // Returns true
```

Functions can also be renamed at import.

```motoko no-repl
// map and find are imported as-is, while foldLeft is renamed to fold.
import { map; find; foldLeft = fold } "mo:base/Array";
```


## Importing local files

Projects may split code into multiple files for better organization, such as:

```
src/project_backend
 ├── main.mo   // Contains the main actor
 ├── types.mo  // Stores type definitions
 ├── utils.mo  // Contains helper functions
```

A prefix is not required for local imports and the `.mo` file extension is omitted. The imported modules must be in the same directory as `main.mo`.

```motoko no-repl
import Types "types";
import Utils "utils";
```

## Importing from another package or directory

Modules can also be imported from other packages or subdirectories.

```motoko no-repl
// Redraw package contains a Render module
import Render "mo:redraw/Render";

// Mono5x5 module is inside the glyph/ subdirectory.
import Mono5x5 "mo:redraw/glyph/Mono5x5";
```

## Importing packages from a package manager

Dependencies are managed using a package manager or defined in the project's `dfx.json` file. Motoko supports package managers like [Mops](https://mops.one/) and [Vessel](https://github.com/dfinity/vessel) to install third-party libraries.

### Configuring the package manager in `dfx.json`

```json
{
  "defaults": {
    "build": {
      "packtool": "mops sources"
    }
  }
}
```

For Vessel, use `"vessel sources"`.

### Installing a package with a package manager

With [Mops](https://mops.one/), add the mops package with `mops add`, then import the package into your Motoko code `import Vec "mo:vector";`.

With [Vessel](https://github.com/dfinity/vessel), add the package to `vessel.dhall`.

## Importing actor classes

When imported, an [actor](https://internetcomputer.org/docs/motoko/fundamentals/actors-async) class provides a type definition describing the class interface and a function that returns an instance of the class.

For example, if you define the following actor class:

```motoko no-repl title="Counters.mo"
persistent actor class Counter(init : Nat) {
  var count = init;

  public func inc() : async () { count += 1 };

  public func read() : async Nat { count };

  public func bump() : async Nat {
    count += 1;
    count;
  };
};
```

It can be imported into another file:

```motoko no-repl
import Counters "Counters";
import Debug "mo:base/Debug";
import Nat "mo:base/Nat";

persistent actor CountToTen {
  public func countToTen() : async () {
    let C : Counters.Counter = await Counters.Counter(1);
    while ((await C.read()) < 10) {
      Debug.print(Nat.toText(await C.read()));
      await C.inc();
    };
  }
};
```

`Counters.Counter(1)` installs a new counter on the network. Installation is [asynchronous](https://internetcomputer.org/docs/motoko/fundamentals/actors-async#async--await), so the result is awaited.  If the actor class is not named, it will result in a bad import error because actor class imports cannot be anonymous.

## Importing from another canister

Actors and their functions can be imported from other [canisters](https://internetcomputer.org/docs/building-apps/essentials/canisters) using the `canister:` prefix.


```motoko no-repl
import BigMap "canister:BigMap";
import Connectd "canister:Connectd";
```

`BigMap` and `Connectd` are separate canisters defined in `dfx.json`. Canister functions are shared and may require `await` to call them.

Unlike a Motoko module, an imported canister:

- Can be implemented in any language that emits a Candid interface.
- Has its type derived from a `.did` file, not from Motoko itself.

## Naming imported modules

While the imported module name usually matches the file name, custom names can be used to avoid conflicts or simplify references.

```motoko no-repl
import List "mo:base/List";
import Sequence "mo:collections/List";
import L "mo:base/List";
```

`List` from Motoko's base library remains `List`. `List` from another package is renamed `Sequence`. `List` is also imported as `L` for convenience.

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />