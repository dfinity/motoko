---
sidebar_position: 3
---

# Stable variables and upgrade methods



One key feature of Motoko is its ability to automatically persist the program's state without explicit user instruction, called **orthogonal persistence**. This not only covers persistence across transactions but also includes canister upgrades. For this purpose, Motoko features a bespoke compiler and runtime system that manages upgrades in a sophisticated way such that a new program version can pick up the state left behind by a previous program version. As a result, Motoko data persistence is not simple but also prevents data corruption or loss, while being efficient at the same time. No database, stable memory API, or stable data structure is required to retain state across upgrades. Instead, a simple `stable` keyword is sufficient to declare an data structure of arbitrary shape persistent, even if the structure uses sharing, has a deep complexity, or contains cycles.

This is substantially different to other languages supported on the IC, which use off-the-shelf language implementations that are not designed for orthogonal persistence in mind: They rearrange memory structures in an uncontrolled manner on re-compilation or at runtime. As an alternative, in other languages, programmers have to explicitly use stable memory or special stable data structures to rescue their data between upgrades. Contrary to Motoko, this approach is not only cumbersome, but also unsafe and inefficient. Compared to using stable data structures, Motoko's orthogonal persistence allows more natural data modeling and significantly faster data access, eventually resulting in more efficient programs.

## Declaring stable variables

In an actor, you can configure which part of the program is considered to be persistent, i.e. survives upgrades, and which part are ephemeral, i.e. are reset on upgrades.

More precisely, each `let` and `var` variable declaration in an actor can specify whether the variable is `stable` or `transient`. If you don’t provide a modifier, the variable is assumed to be `transient` by default.


The semantics of the modifiers is as follows:
* `stable` means that all values directly or indirectly reachable from that stable actor variable are considered persistent and automatically retained across upgrades. This is the primary choice for most of the program's state.
* `transient` means that the variable is re-initialized on upgrade, such that the values referenced by this transient variable can be discarded, unless the values are transitively reachable by other variables that are stable. `transient` is only used for temporary state or references to high-order types, such as local function references, see [stable types](#stable-types).

:::note

Previous versions of Motoko (up to version 0.13.4) used the keyword `flexible` instead of `transient`. Both keywords are accepted interchangeably but the legacy `flexible` keyword may be deprecated in the future.

:::note

The following is a simple example of how to declare a stable counter that can be upgraded while preserving the counter’s value:

``` motoko file=../examples/StableCounter.mo
```

Starting with Motoko v0.13.5, if you prefix the `actor` keyword with the keyword `persistent`, then all `let` and `var` declarations of the actor or actor class are implicitly declared `stable`. Only `transient` variables will need an explicit `transient` declaration.
Using a `persistent` actor can help avoid unintended data loss. It is the recommended declaration syntax for actors and actor classes. The non-`persistent` declaration is provided for backwards compatibility.

Since Motoko v0.13.5, the recommended way to declare `StableCounter` above is:

``` motoko file=../examples/PersistentCounter.mo
```

:::note

You can only use the `stable`, `transient` (or legacy `flexible`) modifier on `let` and `var` declarations that are **actor fields**. You cannot use these modifiers anywhere else in your program.

:::


When you first compile and deploy a canister, all transient and stable variables in the actor are initialized in sequence. When you deploy a canister using the `upgrade` mode, all stable variables that existed in the previous version of the actor are pre-initialized with their old values. After the stable variables are initialized with their previous values, the remaining transient and newly-added stable variables are initialized in sequence.

:::danger
Do not forget to declare variables `stable` if they should survive canister upgrades as the default is `transient` if no modifier is declared.
A simple precaution is declare the entire actor or actor class `persistent`.
:::

## Persistence modes

Motoko currently features two implementations for orthogonal persistence, see [persistence modes](orthogonal-persistence/modes.md).

## Stable types

Because the compiler must ensure that stable variables are both compatible with and meaningful in the replacement program after an upgrade, every `stable` variable must have a stable type. A type is stable if the type obtained by ignoring any `var` modifiers within it is shared.

The only difference between stable types and shared types is the former’s support for mutation. Like shared types, stable types are restricted to first-order data, excluding local functions and structures built from local functions (such as class instances). This exclusion of functions is required because the meaning of a function value, consisting of both data and code, cannot easily be preserved across an upgrade. The meaning of plain data, mutable or not, can be.

:::note

In general, classes are not stable because they can contain local functions. However, a plain record of stable data is a special case of object types that are stable. Moreover, references to actors and shared functions are also stable, allowing you to preserve their values across upgrades. For example, you can preserve the state record of a set of actors or shared function callbacks subscribing to a service.

:::

## Converting non-stable types into stable types

For variables that do not have a stable type, there are two options for making them stable:

1. Use a `stable` module for the type, such as:

  - [StableBuffer](https://github.com/canscale/StableBuffer)
  - [StableHashMap](https://github.com/canscale/StableHashMap)
  - [StableRBTree](https://github.com/canscale/StableRBTree)

:::note
Unlike stable data structures in the Rust CDK, these modules do not use stable memory but rely on orthogonal persistence. The adjective "stable" only denotes a stable type in Motoko.
:::

2. Extract the state in a stable type, and wrap it in the non-stable type.

For example, the stable type `TemperatureSeries` covers the persistent data, while the non-stable type `Weather` wraps this with additional methods (local function types).


``` motoko no-repl file=../examples/WeatherActor.mo
```

3. __Discouraged and not recommended__: [Pre- and post-upgrade hooks](#preupgrade-and-postupgrade-system-methods) allow copying non-stable types to stable types during upgrades. This approach is error-prone and does not scale for large data. **Per best practices, using these methods should be avoided if possible.** Conceptually, it also does not align well with the idea of orthogonal persistence.

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

A stable signature `<stab-sig1>` is stable-compatible with signature `<stab-sig2>`, if for each stable field `<id> : T` in `<stab-sig1>` one of the following conditions hold:

- `<stab-sig2>` does not contain a stable field `<id>`.
- `<stab-sig>` has a matching stable field `<id> : U` with `T <: U`.

Note that `<stab-sig2>` may contain additional fields or abandon fields of `<stab-sig1>`. Mutability can be different for matching fields.

`<stab-sig1>` is the signature of an older version while `<stab-sig2>` is the signature of a newer version.

The subtyping condition on stable fields ensures that the final value of some field can be consumed as the initial value of that field in the upgraded code.

:::tip

You can check the stable-compatibility of two `.most` files containing stable signatures, using `moc` compiler option `--stable-compatible file1.most file2.most`.

:::


## Upgrade safety

When upgrading a canister, it is important to verify that the upgrade can proceed without:

-   Introducing an incompatible change in stable declarations.
-   Breaking clients due to a Candid interface change.

With [enhanced orthogonal persistence](orthogonal-persistence/enhanced.md), Motoko rejects incompatible changes of stable declarations during upgrade attempt.
Moreover, `dfx` checks the two conditions before attempting the upgrade and warns users correspondingly.

A Motoko canister upgrade is safe provided:

-  The canister’s Candid interface evolves to a Candid subtype.
-  The canister’s Motoko stable signature evolves to a stable-compatible one.

:::danger
With [classical orthogonal persistence](orthogonal-persistence/classical.md), the upgrade can still fail due to resource constraints. This is problematic as the canister can then not be upgraded. It is therefore strongly advised to test the scalability of upgrades well. Enhanced orthogonal persistence will abandon this issue.
:::

:::tip

You can check valid Candid subtyping between two services described in `.did` files using the [`didc` tool](https://github.com/dfinity/candid) with argument `check file1.did file2.did`.

:::

## Upgrading a deployed actor or canister

After you have deployed a Motoko actor with the appropriate `stable` variables, you can use the `dfx deploy` command to upgrade an already deployed version. For information about upgrading a deployed canister, see [upgrade a canister smart contract](/docs/current/developer-docs/smart-contracts/maintain/upgrade).

`dfx deploy` checks that the interface is compatible, and if not, shows this message and asks if you want to continue:

```
You are making a BREAKING change. Other canisters or frontend clients relying on your canister may stop working.
```

In addition, Motoko with enhanced orthogonal persistence implements extra safe guard in the runtime system to ensure that the stable data is compatible, to exclude any data corruption or misinterpretation. Moreover, `dfx` also warns about dropping stable variables.

## Data migration

Often, data representation changes with a new program version. For orthogonal persistence, it is important the language is able to allow flexible data migration to the new version.

Motoko supports two kinds of data migrations: Implicit migration and explicit migration.

### Implicit migration

This is automatically supported when the new program version is stable-compatible with the old version. The runtime system of Motoko then automatically handles the migration on upgrade.

More precisely, the following changes can be implicitly migrated:
* Adding or removing actor fields.
* Changing mutability of the actor field.
* Removing record fields.
* Adding variant fields.
* Changing `Nat` to `Int`.
* Shared function parameter contravariance and return type covariance.
* Any change that is allowed by the Motoko's subtyping rule.

### Explicit migration

Any more complex migration is possible by user-defined functionality.

For this purpose, a three step approach is taken:
1. Introduce new variables of the desired types, while keeping the old declarations.
2. Write logic to copy the state from the old variables to the new variables on upgrade.
3. Drop the old declarations once all data has been migrated.

For more information, see the [example of explicit migration](compatibility.md#explicit-migration).

## Legacy features

The following aspects are retained for historical reasons and backwards compatibility:

### Pre-upgrade and post-upgrade system methods

:::danger
Using the pre- and post-upgrade system methods is discouraged. It is error-prone and can render a canister unusable. In particular, if a `preupgrade` method traps and cannot be prevented from trapping by other means, then your canister may be left in a state in which it can no longer be upgraded.  Per best practices, using these methods should be avoided if possible.
:::

Motoko supports user-defined upgrade hooks that run immediately before and after an upgrade. These upgrade hooks allow triggering additional logic on upgrade.
These hooks are declared as `system` functions with special names, `preugrade` and `postupgrade`. Both functions must have type `: () → ()`.

:::danger
If `preupgrade` raises a trap, hits the instruction limit, or hits another IC computing limit, the upgrade can no longer succeed and the canister is stuck with the existing version.
:::

:::tip
`postupgrade` is not needed, as the equal effect can be achieved by introducing initializing expressions in the actor, e.g. non-stable `let` expressions or expression statements.
:::

### Stable memory and stable regions

Stable memory was introduced on the IC to allow upgrades in languages that do not implement orthogonal persistence of the main memory. This is the case with Motoko's classical persistence as well as other languages besides Motoko.

Stable memory and stable regions can still be used in combination with orthogonal persistence, although there is little practical need for this with enhanced orthogonal persistence and the future large main memory capacity on the IC.

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />
