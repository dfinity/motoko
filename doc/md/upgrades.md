# Stable variables and upgrade methods

One key feature of the Internet Computer is its ability to persist canister smart contract state using WebAssembly memory and globals rather than a traditional database. This means that that the entire state of a canister is magically restored before, and saved after, each message, without explicit user instruction. This automatic and user-transparent preservation of state is called *orthogonal persistence*.

Though convenient, orthogonal persistence poses a challenge when it comes to upgrading the code of a canister. Without an explicit representation of the canister’s state, how does one tranfer any application data from the retired canister to its replacement?

Accommodating upgrades without data loss requires some new facility to *migrate* a canister’s crucial data to the upgraded canister. For example, if you want to deploy a new version of a user-registration canister to fix an issue or add functionality, you need to ensure that existing registrations survive the upgrade process.

The Internet Computer’s persistence model allows a canister to save and restore such data to dedicated *stable memory* that, unlike ordinary canister memory, is retained across an upgrade, allowing a canister to transfer data in bulk to its replacement canister.

For applications written in Motoko, the language provides high-level support for preserving state that leverages Internet Computer stable memory. This higher-level feature, called *stable storage*, is designed to accommodate changes to both the application data and to the Motoko compiler used to produce the application code.

Utilizing stable storage depends on you — as the application programmer — anticipating and indicating the data you want to retain after an upgrade. Depending on the application, the data you decide to persist might be some, all, or none of a given actor’s state.

## Declaring stable variables

In an actor, you can nominate a variable for stable storage (in Internet Computer stable memory) by using the `stable` keyword as a modifier in the variable’s declaration.

More precisely, every `let` and `var` variable declaration in an actor can specify whether the variable is `stable` or `flexible`. If you don’t provide a modifier, the variable is declared as `flexible` by default.

The following is a simple example of how to declare a stable counter that can be upgraded while preserving the counter’s value:

``` motoko
actor Counter {

  stable var value = 0;

  public func inc() : async Nat {
    value += 1;
    return value;
  };
}
```

<div class="note">

You can only use the `stable` or `flexible` modifier on `let` and `var` declarations that are **actor fields**. You cannot use these modifiers anywhere else in your program.

</div>

## Typing

Because the compiler must ensure that stable variables are both compatible with and meaningful in the replacement program after an upgrade, the following type restrictions apply to stable state:

-   every `stable` variable must have a *stable* type

where a type is *stable* if the type obtained by ignoring any `var` modifiers within it is *shared*.

Thus the only difference between stable types and shared types is the former’s support for mutation. Like shared types, stable types are restricted to first-order data, excluding local functions and structures built from local functions (such as objects). This exclusion of functions is required because the meaning of a function value — consisting of both data and code — cannot easily be preserved across an upgrade, while the meaning of plain data — mutable or not — can be.

<div class="note">

In general, object types are not stable because they can contain local functions. However, a plain record of stable data is a special case of object types that is stable. Moreover, references to actors and shared functions are also stable, allowing you to preserve their values across upgrades. For example, you can preserve state recording a set of actors or shared function callbacks subscribing to a service.

</div>

## How stable variables are upgraded

When you first compile and deploy a canister, all flexible and stable variables in the actor are initialized in sequence. When you deploy a canister using the `upgrade` mode, all stable variables that existed in the previous version of the actor are pre-initialized with their old values. After the stable variables are initialized with their previous values, the remaining flexible and newly-added stable variables are initialized in sequence.

## Preupgrade and postupgrade system methods

Declaring a variable to be `stable` requires its type to be stable too. Since not all types are stable, some variables cannot be declared `stable`.

As a simple example, consider the Registry\` actor from the discussion of [orthogonal persistence](motoko.md#orthogonal_persistence).

``` motoko
import Text "mo:base/Text";
import Map "mo:base/HashMap";

actor Registry {

  let map = Map.HashMap<Text, Nat>(10, Text.equal, Text.hash);

  public func register(name : Text) : async () {
    switch (map.get(name)) {
      case null {
        map.put(name, map.size());
      };
      case (?id) { };
    }
  };

  public func lookup(name : Text) : async ?Nat {
    map.get(name);
  };
};

await Registry.register("hello");
(await Registry.lookup("hello"), await Registry.lookup("world"))
```

This actor assigns sequential identifiers to `Text` values, using the size of the underlying `map` object to determine the next identifier. Like other actors, it relies on *orthogonal persistence* to maintain the state of the hashmap between calls.

We’d like to make the `Register` upgradable, without the upgrade losing any existing registrations.

Unfortunately, its state, `map`, has a proper object type that contains member functions (for example, `map.get`), so the `map` variable cannot, itself, be declared `stable`.

For scenarios like this that can’t be solved using stable variables alone, Motoko supports user-defined upgrade hooks that, when provided, run immediately before and after upgrade. These upgrade hooks allow you to migrate state between unrestricted flexible variables to more restricted stable variables. These hooks are declared as `system` functions with special names, `preugrade` and `postupgrade`. Both functions must have type `: () → ()`.

The `preupgrade` method lets you make a final update to stable variables, before the runtime commits their values to Internet Computer stable memory, and performs an upgrade. The `postupgrade` method is run after an upgrade has initialized the replacement actor, including its stable variables, but before executing any shared function call (or message) on that actor.

Here, we introduce a new stable variable, `entries`, to save and restore the entries of the unstable hash table.

``` motoko
import Text "mo:base/Text";
import Map "mo:base/HashMap";
import Array "mo:base/Array";
import Iter "mo:base/Iter";

actor Registry {

  stable var entries : [(Text, Nat)] = [];

  let map = Map.fromIter<Text,Nat>(
    entries.vals(), 10, Text.equal, Text.hash);

  public func register(name : Text) : async () {
    switch (map.get(name)) {
      case null  {
        map.put(name, map.size());
      };
      case (?id) { };
    }
  };

  public func lookup(name : Text) : async ?Nat {
    map.get(name);
  };

  system func preupgrade() {
    entries := Iter.toArray(map.entries());
  };

  system func postupgrade() {
    entries := [];
  };
}
```

Note that the type of `entries`, being just an array of `Text` and `Nat` pairs, is indeed a stable type.

In this example, the `preupgrade` system method simply writes the current `map` entries to `entries` before `entries` is saved to stable memory. The `postupgrade` system method resets `entries` to the empty array after `map` has been populated from `entries` to free space.

## Stable type signatures

The collection of stable variable declarations in an actor can be summarized in a *stable signature*.

The textual representation of an actor’s stable signature resembles the internals of a Motoko actor type:

``` motoko
actor {
  stable x : Nat;
  stable var y : Int;
  stable z : [var Nat];
};
```

It specifies the names, types and mutability of the actor’s stable fields, possibly preceded by relevant Motoko type declarations.

<div class="tip">

You can emit the stable signature of the main actor or actor class to a `.most` file using `moc` compiler option `--stable-types`. You should never need to author your own `.most` file.

</div>

A stable signature `<stab-sig1>` is *stable-compatible* with signature `<stab-sig2>`, if, and only,

-   every immutable field `stable <id> : T` in `<stab-sig1>` has a matching field `stable <id> : U` in `<stab-sig2>` with `T <: U`.

-   every mutable field `stable var <id> : T` in `<stab-sig1>` has a matching field `stable var <id> : U` in `<stab-sig2>` with `T <: U`.

Note that `<stab-sig2>` may contain additional fields. Typically, `<stab-sig1>` is the signature of an older version while `<stab-sig2>` is the signature of a newer version.

The subtyping condition on stable fields ensures that the final value of some field can be consumed as the initial value of that field in the upgraded code.

<div class="tip">

You can check the stable-compatiblity of two `.most` files, `cur.most` and `nxt.most` (containing stable signatures), using `moc` compiler option `--stable-compatible cur.most nxt.most`.

</div>

<div class="note">

The *stable-compatible* relation is quite conservative. In the future, it may be relaxed to accommodate a change in field mutability and/or abandoning fields from `<stab-sig1>` (but with a warning).

</div>

## Upgrade safety

Before upgrading a deployed canister, you should ensure that the upgrade is safe and will not

-   break existing clients (due to a Candid interface change); or

-   discard Motoko stable state (due to an incompatible change in stable declarations).

A Motoko canister upgrade is safe provided:

-   the canister’s Candid interface evolves to a Candid subtype; and

-   the canister’s Motoko stable signature evolves to a *stable-compatible* one.

Upgrade safety does not guarantee that the upgrade process will succeed (it can still fail due to resource constraints). However, it should at least ensure that a successful upgrade will not break Candid type compatibility with existing clients or unexpectedly lose data that was marked `stable`.

<div class="tip">

You can check valid Candid subtyping between two services described in `.did` files, `cur.did` and `nxt.did` (containing Candid types), using the `didc` tool with argument `check nxt.did cur.did`. The `didc` tool is available at <https://github.com/dfinity/candid>.

</div>

## Metadata sections

The Motoko compiler embeds the Candid interface and stable signature of a canister as canister metadata, recorded in additional Wasm custom sections of a compiled binary.

This metadata can be selectively exposed by the IC and used by tools such as `dfx` to verify upgrade compatibility.

## Upgrading a deployed actor or canister smart contract

After you have deployed a Motoko actor with the appropriate `stable` variables or `preupgrade` and `postupgrade` system methods, you can use the `dfx canister install` command with the `--mode=upgrade` option to upgrade an already deployed version. For information about upgrading a deployed canister, see [Upgrade a canister smart contract](../developers-guide/working-with-canisters.md#upgrade-canister).

An upcoming version of `dfx` will, if appropriate, check the safety of an upgrade by comparing the Candid and (for Motoko canisters only) the stable signatures embedded in the deployed binary and upgrade binary, and abort the upgrade request when unsafe.
