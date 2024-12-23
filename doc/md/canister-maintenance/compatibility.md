---
sidebar_position: 4
---

# Verifying upgrade compatibility



When upgrading a canister, it is important to verify that the upgrade can proceed without:

-   Introducing an incompatible change in stable declarations.
-   Breaking clients due to a Candid interface change.

`dfx` checks these properties statically before attempting the upgrade.
Moreover, with [enhanced orthogonal persistence](orthogonal-persistence/enhanced.md), Motoko rejects incompatible changes of stable declarations.

## Upgrade example

The following is a simple example of how to declare a stateful counter:

``` motoko no-repl file=../examples/count-v0.mo
```

Importantly, in this example, when the counter is upgraded, its state is lost.
This is because actor variables are by default `transient`, meaning they get reinitialized on an upgrade.
The above actor is equivalent to using the `transient` declaration:

``` motoko no-repl file=../examples/count-v0transient.mo
```


To fix this, you can declare a `stable` variable that is retained across upgrades:


``` motoko no-repl file=../examples/count-v1stable.mo
```

To make `stable` the default for all declarations and `transient` optional, you can prefix the actor declaration with the keyword `persistent`.

``` motoko no-repl file=../examples/count-v1.mo
```

If the variable `state` were not declared `stable`, either explicitly or by applying `persistent` to the `actor` keyword, `state` would restart from `0` on upgrade.

## Evolving the stable declarations

Changing counter from `Nat` to `Int` is a compatible change in stable declarations. The counter value is retained during the upgrade.

``` motoko no-repl file=../examples/count-v2.mo
```

## Stable type signatures

A stable type signature describes the stable content of a Motoko actor.
You can think of this as the interior interface of the actor, that it presents to its future upgrades.

For example, `v1`'s stable types:

``` motoko no-repl file=../examples/count-v1.most
```

An upgrade from `v1` to `v2`'s stable types consumes a [`Nat`](../base/Int.md) as an [`Int`](../base/Nat.md), which is valid because `Int <: Nat`.

``` motoko no-repl file=../examples/count-v2.most
```

## Evolving the Candid interface

In this extension of the interface, old clients remain satisfied, while new ones get extra features such as the `decrement` function and the `read` query in this example.

``` motoko no-repl file=../examples/count-v3.mo
```

## Dual interface evolution

An upgrade is safe provided that both the Candid interface and stable type signatures remain compatible:
* Each stable variable must either be newly declared, dropped, or re-declared at a supertype of its old type.
* The Candid interface evolves to a subtype.

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

## Incompatible upgrade

Let's take a look at another example where the counter's type is again changed, this time from [`Int`](../base/Int.md) to [`Nat`](../base/Float.md):

``` motoko no-repl file=../examples/count-v4.mo
```

This version is neither compatible to the Candid interface nor to the stable type declarations.
- Since `Float </: Int`, the new type of `state` is not compatible to the old type.
- The return type change of `read` is also not valid.

Motoko rejects upgrades with incompatible state changes with [enhanced orthogonal persistence](orthogonal-persistence/enhanced.md).
This is to guarantee that the stable state is always kept safe.

```
Error from Canister ...: Canister called `ic0.trap` with message: RTS error: Memory-incompatible program upgrade.
```

In addition to Motoko's check, `dfx` raises a warning message for these incompatible changes, including the breaking Candid change.

:::danger
Versions of Motoko using [classical orthogonal persistence](orthogonal-persistence/classical.md) will drop the state and reinitialize the counter with `0.0`, if the `dfx` warning is ignored.
:::

## Explicit migration

There is always a migration path to change structure of stable state, even if a direct type change is not compatible.

For this purpose, a user-instructed migration can be done in three steps:

1. Introduce new variables of the desired types, while keeping the old declarations.
2. Write logic to copy the state from the old variables to the new variables on upgrade.

    While the previous attempt of changing state from [`Int`](../base/Int.md) to [`Nat`](../base/Float.md) was invalid, you now can realize the desired change as follows:

    ``` motoko no-repl file=../examples/count-v5.mo
    ```

    To also keep the Candid interface, the `readFloat` has been added, while the old `read` is retired by keeping its declaration and raising a trap internally.

3. Drop the old declarations once all data has been migrated:

    ``` motoko no-repl file=../examples/count-v6.mo
    ```

Alternatively, the type of `state` can be changed to `Any`, also implying that this variable is no longer used.

## Upgrade tooling

`dfx` incorporates an upgrade check. For this purpose, it uses the Motoko compiler (`moc`) that supports:

-   `moc --stable-types …​`: Emits stable types to a `.most` file.

-   `moc --stable-compatible <pre> <post>`: Checks two `.most` files for upgrade compatibility.

Motoko embeds `.did` and `.most` files as Wasm custom sections for use by `dfx` or other tools.

To upgrade e.g. from `cur.wasm` to `nxt.wasm`, `dfx` checks that both the Candid interface and stable variables are compatible:

```
didc check nxt.did cur.did  // nxt <: cur
moc --stable-compatible cur.most nxt.most  // cur <<: nxt
```

Using the versions above, the upgrade from `v3` to `v4` fails this check:

```
> moc --stable-compatible v3.most v4.most
(unknown location): Compatibility error [M0170], stable variable state of previous type
  var Int
cannot be consumed at new type
  var Float
```

With [enhanced orthogonal persistence](orthogonal-persistence/enhanced.md), compatibility errors of stable variables are always detected in the runtime system and if failing, the upgrade is safely rolled back.

:::danger
With [classical orthogonal persistence](orthogonal-persistence/classical.md), however, an upgrade attempt from `v2.wasm` to `v3.wasm` is unpredictable and may lead to partial or complete data loss if the `dfx` warning is ignored.
:::

## Adding record fields

A common, real-world example of an incompatible upgrade can be found [on the forum](https://forum.dfinity.org/t/questions-about-data-structures-and-migrations/822/12?u=claudio/).

In that example, a user was attempting to add a field to the record payload of an array, by upgrading from stable type interface:

``` motoko no-repl
persistent actor {
  type Card = {
    title : Text;
  };
  var map : [(Nat32, Card)] = [(0, { title = "TEST"})];
};
```

to *incompatible* stable type interface:

``` motoko no-repl
persistent actor {
  type Card = {
    title : Text;
    description : Text;
  };
  var map : [(Nat32, Card)] = [];
};
```

### Problem

When trying this upgrade, `dfx` issues the following warning:

```
Stable interface compatibility check issued an ERROR for canister ...
Upgrade will either FAIL or LOSE some stable variable data.

(unknown location): Compatibility error [M0170], stable variable map of previous type
  var [(Nat32, Card)]
cannot be consumed at new type
  var [(Nat32, Card__1)]

Do you want to proceed? yes/No
```
It is recommended not to continue, as you will lose the state in older versions of Motoko that use [classical orthogonal persistence](orthogonal-persistence/classical.md).
Upgrading with [enhanced orthogonal persistence](orthogonal-persistence/enhanced.md) will trap and roll back, keeping the old state.

Adding a new record field to the type of existing stable variable is not supported. The reason is simple: The upgrade would need to supply values for the new field out of thin air. In this example, the upgrade would need to conjure up some value for the `description` field of every existing `card` in `map`. Moreover, allowing adding optional fields is also a problem, as a record can be shared from various variables with different static types, some of them already declaring the added field or adding a same-named optional field with a potentially different type (and/or different semantics).

### Solution

To resolve this issue, an [explicit migration](#explicit-migration) is needed:

1. You must keep the old variable `map` with the same structural type. However, you are allowed to change type alias name (`Card` to `OldCard`).
2. You can introduce a new variable `newMap` and copy the old state to the new one, initializing the new field as needed.
3. Then, upgrade to this new version.

``` motoko no-repl
import Array "mo:base/Array";

persistent actor {
  type OldCard = {
    title : Text;
  };
  type NewCard = {
    title : Text;
    description : Text;
  };
  var map : [(Nat32, OldCard)] = [];
  var newMap : [(Nat32, NewCard)] = Array.map<(Nat32, OldCard), (Nat32, NewCard)>(
    map,
    func(key, { title }) { (key, { title; description = "<empty>" }) },
  );
};
```

4. **After** we have successfully upgraded to this new version, we can upgrade once more to a version, that drops the old `map`.

``` motoko no-repl
persistent actor {
  type Card = {
    title : Text;
    description : Text;
  };
  var newMap : [(Nat32, Card)] = [];
};
```

`dfx` will issue a warning that `map` will be dropped.

Make sure, you have previously migrated the old state to `newMap` before applying this final reduced version.

```
Stable interface compatibility check issued a WARNING for canister ...
(unknown location): warning [M0169], stable variable map of previous type
  var [(Nat32, OldCard)]
 will be discarded. This may cause data loss. Are you sure?
```

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />
