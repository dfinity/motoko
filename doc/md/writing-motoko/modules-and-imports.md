---
sidebar_position: 16
---

# Modules and imports



The design of Motoko strives to minimize built-in types and operations. Instead of built-in types, Motoko provides a base library of modules to handle many kinds of common operations and make the language feel complete. This base library is still evolving with modules that support core features, and the base library APIs are subject to change over time to varying degrees. You should note, in particular, that the size and number of modules and functions included in the base library is likely to increase dramatically. Updates to the base library modules might introduce breaking changes that require you to update your programs to remain compatible. Breaking changes are communicated through the [Motoko migration guides](../migration-guides/overview.md).

This section provides examples of different scenarios for using the `module` and `import` keywords.

## Importing from the base library

View the [online documentation for the Motoko base library](../base/index.md).

You can find source code for the Motoko base modules in the open source [repository](https://github.com/dfinity/motoko-base).

There are instructions in the repository for generating a local copy of the current documentation for the Motoko base package.

To import from the base library, use the `import` keyword followed by a local module name and a URL where the `import` declaration can find the module. For example:

``` motoko
import Debug "mo:base/Debug";
Debug.print("hello world");
```

This example illustrates how to import Motoko code—indicated by using the `mo:` prefix to identify the module as a Motoko module. The declaration does not include the `.mo` file type extension. Then, it uses the `base/` base library path and the module name [`Debug`](../base/Debug.md).

You can also selectively import a subset of named values from a module by using the object pattern syntax:

``` motoko
import { map; find; foldLeft = fold } = "mo:base/Array";
```

In this example, the functions `map` and `find` are imported unaltered, while the `foldLeft` function is renamed to `fold`.

## Importing local files

Another common approach to writing programs in Motoko involves splitting up the source code into different modules. For example, you might design an application to use the following model:

-   A `main.mo` file to contain the actor and functions that change state.

-   A `types.mo` file for all of your custom type definitions.

-   A `utils.mo` file for functions that do work outside of the actor.

In this scenario, you might place all three files in the same directory and use a local import to make the functions available where they are needed.

For example, the `main.mo` contains the following lines to reference the modules in the same directory:

``` motoko no-repl
import Types "types";
import Utils "utils";
```

Because these lines import modules from the local project instead of the Motoko library, these import declarations don’t use the `mo:` prefix.

In this example, both the `types.mo` and `utils.mo` files are in the same directory as the `main.mo` file. Once again, import does not use the `.mo` file suffix.

## Importing from another package or directory

You can also import modules from other packages or from directories other than the local directory.

For example, the following lines import modules from a `redraw` package that is defined as a dependency:

``` motoko no-repl
import Render "mo:redraw/Render";
import Mono5x5 "mo:redraw/glyph/Mono5x5";
```

You can define dependencies for a project using a package manager or in the project `dfx.json` configuration file.

In this example, the `Render` module is in the default location for source code in the `redraw` package and the `Mono5x5` module is in a `redraw` package subdirectory called `glyph`.

## Importing packages from a package manager

To download and install third-party packages, a package manager such as [Mops](https://mops.one) or [Vessel](https://github.com/dfinity/vessel) can be used.

To use either package manager, edit your project's `dfx.json` file to specify a `packtool`, such as:

```json
{
  "defaults": {
    "build": {
      "packtool": "mops sources"
    }
  }
}
```

For Vessel, use `vessel sources`.

Then, to download a package with the `mops` CLI tool, use a command such as:

```
mops add vector
```

For Vessel, edit the `vessel.dhall` file to include what packages your project will import.

Then, import the packages as you would import other packages in the Motoko source file:

```motoko no-repl
import Vec "mo:vector";
import Vec "mo:vector/Class";
```

## Importing actor classes

While module imports are typically used to import libraries of local functions and values, they can also be used to import actor classes. When an imported file consists of a named actor class, the client of the imported field sees a module containing the actor class.

This module has two components, both named after the actor class:

-   A type definition describing the interface of the class.

-   An asynchronous function that takes the class parameters as arguments an asynchronously returns a fresh instance of the class.

For example, a Motoko actor can import and instantiate the `Counter` class described as follows:

`Counters.mo`:

``` motoko name=Counters file=../examples/Counters.mo
```

`CountToTen.mo`:

``` motoko no-repl file=../examples/CountToTen.mo
```

The call to `Counters.Counter(1)` installs a fresh counter on the network. Installation is asynchronous, so the caller must `await` the result.

The type annotation `: Counters.Counter` is redundant here. It’s included only to illustrate that the type of the actor class is available when required.

## Importing from another canister smart contract

You can also import actors and their shared functions from another canister by using the `canister:` prefix in place of the `mo:` prefix.

:::note

Unlike a Motoko library, an imported canister can be implemented in any other language, such as Rust or even a different Motoko version, that emits Candid interfaces for its canister.

:::

For example, you might have a project that produces the following three canisters:

-   BigMap (implemented in Rust).

-   Connectd (implemented in Motoko).

-   LinkedUp (implemented in Motoko).

These three canisters are declared in the project’s `dfx.json` configuration file and compiled by running `dfx build`.

You can use the following lines to import the `BigMap` and `Connectd` canisters as actors in the Motoko LinkedUp actor:

``` motoko no-repl
import BigMap "canister:BigMap";
import Connectd "canister:connectd";
```

When importing canisters, it is important to note that the type for the imported canister corresponds to a **Motoko actor** instead of a **Motoko module**. This distinction can affect how some data structures are typed.

For the imported canister actor, types are derived from the Candid `project-name.did` file for the canister, rather than from Motoko itself.

The translation from Motoko actor type to Candid service type is typically one-to-one, and there are some distinct Motoko types that map to the same Candid type. For example, the Motoko [`Nat32`](../base/Nat32.md) and `Char` types are both exported as Candid type [`Nat32`](../base/Nat32.md), but [`Nat32`](../base/Nat32.md) is canonically imported as Motoko [`Nat32`](../base/Nat32.md), not `Char`.

The type of an imported canister function might differ from the type of the original Motoko code that implements it. For example, if the Motoko function had type `shared Nat32 -> async Char` in the implementation, its exported Candid type would be `(nat32) -> (nat32)`, but the Motoko type imported from this Candid type will actually be the correct type `shared Nat32 -> async Nat32`.

## Naming imported modules

Although the most common convention is to identify imported modules by the module name as illustrated in the examples above, there’s no requirement for you to do so. For example, you might want to use different names to avoid naming conflicts or to simplify the naming scheme.

The following example illustrates different names you might use when importing the `List` base library module, avoiding a clash with another `List` library from a fictional `collections` package:

``` motoko no-repl
import List "mo:base/List:";
import Sequence "mo:collections/List";
import L "mo:base/List";
```

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />