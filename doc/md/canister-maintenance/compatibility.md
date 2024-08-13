---
sidebar_position: 4
---

# Verifying upgrade compatibility

## Overview

When upgrading a canister, it is important to verify that an upgrade can proceed without:

-   Breaking clients due to a Candid interface change.

-   Discarding the Motoko stable state due to a change in stable declarations.

Motoko checks these properties statically before attempting the upgrade.

## Upgrade example

The following is a simple example of how to declare a stateful counter:

``` motoko no-repl file=../examples/count-v0.mo
```

In this example, when the counter is upgraded, its state is lost.

To fix this, you can declare a stable variable that is retained across upgrades:


``` motoko no-repl file=../examples/count-v1.mo
```

If the variable `state` were not declared `stable`, `state` would restart from `0` on upgrade.


## Evolving the Candid interface

In this extension of our interface, old clients remain satisfied, while new ones get extra features such as the `read` query in this example.

``` motoko no-repl file=../examples/count-v2.mo
```

## Changing the stable interface

Let's take a look at an example where the counter is refactored from using [`Int`](../base/Int.md) to [`Nat`](../base/Nat.md).

``` motoko no-repl file=../examples/count-v3.mo
```

Now, the code has been upgraded, but the counter value is back to `0`. The state was lost in an upgrade.

This is because the Candid interface evolved safely​ but the stable types did not.

An upgrade must be able to:

-   Consume any stable variable value from its predecessor, or

-   Run the initializer for a new stable variable.

Since `Int </: Nat`, the upgrade logic discards the saved [`Int`](../base/Int.md) and re-runs the initializer instead. The upgrade silently "succeeded", resetting the counter to `0`.

## Stable type signatures

A stable type signature describes the stable content of a Motoko actor.
You can think of this as the interior interface of the actor, that it presents to its future upgrades.

For example, `v2`'s stable types:

``` motoko no-repl file=../examples/count-v2.most
```

An upgrade from `v2` to `v3`'s stable types requires consuming an [`Int`](../base/Int.md) as a [`Nat`](../base/Nat.md), which is a **type error**.
For example, `-1` is an [`Int`](../base/Int.md), but is not a [`Nat`](../base/Nat.md).

``` motoko no-repl file=../examples/count-v3.most
```

## Dual interface evolution

An upgrade is safe provided that the Candid interface evolves to a subtype and the stable interface evolves to a compatible one: a stable variable must either be newly declared, or re-declared at a supertype of its old type.

Consider the following four versions of the counter example:

Version `v0` with Candid interface `v0.did` and stable type interface `v0.most`:

``` candid file=../examples/count-v0.did
```

``` motoko no-repl file=../examples/count-v0.most
```

Version `v1` with Candid interface `v1.did` and stable type interface `v1.most`,

``` candid file=../examples/count-v1.did
```

``` motoko no-repl file=../examples/count-v1.most
```

Version `v2` with Candid interface `v2.did` and stable type interface `v2.most`,

``` candid file=../examples/count-v2.did
```

``` motoko no-repl file=../examples/count-v2.most
```

Version `v3` with Candid interface `v3.did` and stable type interface `v3.most`:

``` candid file=../examples/count-v3.did
```

``` motoko no-repl file=../examples/count-v3.most
```

## Upgrade tooling

The Motoko compiler (`moc`) supports:

-   `moc --stable-types …​`: Emits stable types to a `.most` file.

-   `moc --stable-compatible <pre> <post>`: Checks two `.most` files for upgrade compatibility.

To upgrade from `cur.wasm` to `nxt.wasm` we need check that both the Candid interface and stable variables are compatible.

```
didc check nxt.did cur.did  // nxt <: cur
moc --stable-compatible cur.most nxt.most  // cur <<: nxt
```

Using the versions above, the upgrade from `v2` to `v3` fails this check:

```
> moc --stable-compatible v2.most v3.most
(unknown location): Compatibility error [M0170], stable variable state of previous type
  var Int
cannot be consumed at new type
  var Nat
```

Because of the compatibility error, you should not attempt to upgrade from `v2.wasm` to `v3.wasm`. The result of upgrading is unpredictable. At best, the upgrade will detect the incompatibility, trap and roll back to the current version, as if the upgrade had never been attempted. At worst, the upgrade will appear to succeed, but lose some or all of the state of the previous version, re-initializing some of the stable variables you intended to preserve.

One way to correctly change the logical state to [`Nat`](../base/Nat.md), is to introduce a new stable variable, `newState`, of type [`Nat`](../base/Nat.md), initialized from the old one (`state`). Unlike the stable signature of v3.wasm, the stable signature of v4.wasm:

``` motoko no-repl file=../examples/count-v4.mo
```

``` motoko no-repl file=../examples/count-v4.most
```

## Incompatible upgrade example

A common, real-world example of an incompatible upgrade can be found [on the forum](https://forum.dfinity.org/t/questions-about-data-structures-and-migrations/822/12?u=claudio/).

In that example, a user was attempting to add a field to the record payload of an array, by upgrading from stable type interface:

``` motoko no-repl
type Card = {
  title : Text
};
actor {
  stable var map: [(Nat32, Card)]
}
```

to *incompatible* stable type interface:

``` motoko no-repl
type Card = {
  title : Text;
  description : Text
};
actor {
  stable var map : [(Nat32, Card)]
}
```

Adding a new record field  does not work. The reason is simple: the upgrade would need to supply values for the new field out of thin air. In this example, the upgrade would need to conjure up some value for the  `description` field of every existing `card` in `map`.

## Metadata sections

Motoko embeds `.did` and `.most` files as Wasm custom sections for use by other tools, e.g. dfx.

`dfx deploy` and `dfx canister install --all --mode upgrade` commands check that the interface is compatible, and if not, show this message and ask if you want to continue:

```
let msg = format!("Candid interface compatibility check failed for canister '{}'.\nYou are making a BREAKING change. Other canisters or frontend clients relying on your canister may stop working.\n\n", canister_info.get_name()) + &err;
```

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />
