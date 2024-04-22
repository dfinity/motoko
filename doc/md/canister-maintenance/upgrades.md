---
sidebar_position: 3
---

# Stable variables and upgrade methods

## Overview

One key feature of ICP is its ability to persist canister state using WebAssembly memory and globals rather than a traditional database. This means that the entire state of a canister is restored before and saved after each message, without explicit user instruction.

A canister saves and restores data to dedicated **stable memory** that, unlike ordinary canister memory, is retained across an upgrade and allows a canister to transfer data in bulk across upgrades.

Motoko provides high-level support for preserving state that leverages stable memory. This feature, called **stable storage**, is designed to accommodate changes to both the application data and the Motoko compiler used to produce the application code.

Utilizing stable storage depends on the developer anticipating and indicating the data to retain after an upgrade. Depending on the application, the data you decide to persist might be some, all, or none of a given actor’s state.

## Declaring stable variables

In an actor, you can configure a variable to use stable storage through the `stable` keyword modifier in the variable’s declaration.

More precisely, every `let` and `var` variable declaration in an actor can specify whether the variable is `stable` or `flexible`. If you don’t provide a modifier, the variable is declared as `flexible` by default.

The following is a simple example of how to declare a stable counter that can be upgraded while preserving the counter’s value:

``` motoko file=../examples/StableCounter.mo
```

:::note

You can only use the `stable` or `flexible` modifier on `let` and `var` declarations that are **actor fields**. You cannot use these modifiers anywhere else in your program.

:::

When you first compile and deploy a canister, all flexible and stable variables in the actor are initialized in sequence. When you deploy a canister using the `upgrade` mode, all stable variables that existed in the previous version of the actor are pre-initialized with their old values. After the stable variables are initialized with their previous values, the remaining flexible and newly-added stable variables are initialized in sequence.

## Typing

Because the compiler must ensure that stable variables are both compatible with and meaningful in the replacement program after an upgrade, every `stable` variable must have a stable type. A type is stable if the type obtained by ignoring any `var` modifiers within it is shared.

The only difference between stable types and shared types is the former’s support for mutation. Like shared types, stable types are restricted to first-order data, excluding local functions and structures built from local functions, such as objects. This exclusion of functions is required because the meaning of a function value, consisting of both data and code, cannot easily be preserved across an upgrade. The meaning of plain data, mutable or not, can be.

:::note

In general, object types are not stable because they can contain local functions. However, a plain record of stable data is a special case of object types that are stable. Moreover, references to actors and shared functions are also stable, allowing you to preserve their values across upgrades. For example, you can preserve the state record of a set of actors or shared function callbacks subscribing to a service.

:::

## Stable type signatures

The collection of stable variable declarations in an actor can be summarized in a stable signature.

The textual representation of an actor’s stable signature resembles the internals of a Motoko actor type:

``` motoko no-repl
actor {
  stable x : Nat;
  stable var y : Int;
  stable z : [var Nat];
};
```

It specifies the names, types and mutability of the actor’s stable fields, possibly preceded by relevant Motoko type declarations.

:::tip

You can emit the stable signature of the main actor or actor class to a `.most` file using `moc` compiler option `--stable-types`. You should never need to author your own `.most` file.

:::

A stable signature `<stab-sig1>` is stable-compatible with signature `<stab-sig2>`, if:

-   Every immutable field `stable <id> : T` in `<stab-sig1>` has a matching field `stable <id> : U` in `<stab-sig2>` with `T <: U`.

-   Every mutable field `stable var <id> : T` in `<stab-sig1>` has a matching field `stable var <id> : U` in `<stab-sig2>` with `T <: U`.

Note that `<stab-sig2>` may contain additional fields. Typically, `<stab-sig1>` is the signature of an older version while `<stab-sig2>` is the signature of a newer version.

The subtyping condition on stable fields ensures that the final value of some field can be consumed as the initial value of that field in the upgraded code.

:::tip

You can check the stable-compatibility of two `.most` files containing stable signatures, using `moc` compiler option `--stable-compatible file1.most file2.most`.

:::

:::note

The stable-compatible relation is quite conservative. In the future, it may be relaxed to accommodate a change in field mutability and/or abandoning fields from `<stab-sig1>` but with a warning.

:::

## Preupgrade and postupgrade system methods

Declaring a variable to be `stable` requires its type to be stable too. Since not all types are stable, some variables cannot be declared `stable`.

As a simple example, consider the following `Registry` actor:

``` motoko file=../examples/Registry.mo
```

This actor assigns sequential identifiers to `Text` values, using the size of the underlying `map` object to determine the next identifier. Like other actors, it relies on orthogonal persistence to maintain the state of the hashmap between calls.

This example would like to make the `Register` upgradable without the upgrade losing any existing registrations, but its state, `map`, has a proper object type that contains member functions, so the `map` variable cannot be declared `stable`.

For scenarios like this that can’t be solved using stable variables alone, Motoko supports user-defined upgrade hooks that run immediately before and after an upgrade. These upgrade hooks allow you to migrate state between unrestricted flexible variables to more restricted stable variables. These hooks are declared as `system` functions with special names, `preugrade` and `postupgrade`. Both functions must have type `: () → ()`.

The `preupgrade` method lets you make a final update to stable variables before the runtime commits their values to stable memory and performs an upgrade. The `postupgrade` method is run after an upgrade has initialized the replacement actor, including its stable variables, but before executing any shared function call or message on that actor.

The following example introduces a new stable variable, `entries`, to save and restore the entries of the unstable hash table:

``` motoko file=../examples/StableRegistry.mo
```

Note that the type of `entries`, being an array of `Text` and `Nat` pairs, is indeed a stable type.

In this example, the `preupgrade` system method writes the current `map` entries to `entries` before `entries` is saved to stable memory. The `postupgrade` system method resets `entries` to the empty array after `map` has been populated from `entries`.

## Upgrade safety

Before upgrading a deployed canister, you should ensure that the upgrade is safe and will not:

-   Break existing clients due to a Candid interface change.

-   Discard Motoko stable state due to an incompatible change in stable declarations.

A Motoko canister upgrade is safe provided:

-   The canister’s Candid interface evolves to a Candid subtype.

-  The canister’s Motoko stable signature evolves to a stable-compatible one.

Upgrade safety does not guarantee that the upgrade process will succeed, as it can still fail due to resource constraints. However, it should at least ensure that a successful upgrade will not break Candid type compatibility with existing clients or unexpectedly lose data that was marked `stable`.

:::tip

You can check valid Candid subtyping between two services described in `.did` files using the [`didc` tool](https://github.com/dfinity/candid) with argument `check file1.did file2.did`.

:::

## Metadata sections

The Motoko compiler embeds the Candid interface and stable signature of a canister as canister metadata, recorded in additional Wasm custom sections of a compiled binary.

This metadata can be selectively exposed by ICP and used by tools such as `dfx` to verify upgrade compatibility.

## Upgrading a deployed actor or canister

After you have deployed a Motoko actor with the appropriate `stable` variables or `preupgrade` and `postupgrade` system methods, you can use the `dfx canister install` command with the `--mode=upgrade` option to upgrade an already deployed version. For information about upgrading a deployed canister, see [upgrade a canister smart contract](/docs/current/developer-docs/smart-contracts/maintain/upgrade).

`dfx canister install --mode=upgrade` checks that the interface is compatible, and if not, show this message and ask if you want to continue:

```
let msg = format!("Candid interface compatibility check failed for canister '{}'.\nYou are making a BREAKING change. Other canisters or frontend clients relying on your canister may stop working.\n\n", canister_info.get_name()) + &err;
```
