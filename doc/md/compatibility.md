# Verifying upgrade compatibility

Goal: we need to verify that an upgrade can proceed without:

-   breaking clients (due to a Candid interface change)

-   discarding Motoko stable state (due to a change in stable declarations)

With Motoko, we promised to check these properties statically (before attempting the upgrade).

Let’s deliver on that promise.

## An unstable counter

The following is a simple example of how to declare a stateful counter.

``` motoko no-repl
actor Counter_v0 {

  var state : Int = 0;

  public func inc() : async Int {
    state += 1;
    return state;
  };

}
```

Unfortunately, when we upgrade this counter (say with itself), its state is lost.

|         |       |         |             |
|---------|-------|---------|-------------|
| version | state | success | call        |
| v0      | 0     | ✓       | inc()       |
| v0      | 1     | ✓       | inc()       |
| v0      | 2     | ✓       | upgrade(v0) |
| v0      | *0*   | *✗*     | inc()       |
| v0      | 1     |         |             |

## A stable counter

In Motoko, we can declare variables to be stable (across upgrades).

``` motoko no-repl
actor Counter_v1 {

  stable var state : Int = 0;

  public func inc() : async Int {
    state += 1;
    return state;
  };
}
```

Because it’s `stable`, this counter’s `state` is *retained* across upgrades.

(If not marked `stable`, `state` would restart from `0` on upgrade).

|         |       |         |             |
|---------|-------|---------|-------------|
| version | state | success | call        |
| v1      | 0     | ✓       | inc()       |
| v1      | 1     | ✓       | inc()       |
| v1      | 2     | ✓       | upgrade(v1) |
| v1      | 2     | *✓*     | inc()       |
| v1      | 3     |         |             |

## Evolving the Candid interface:

Let’s extend the API - old clients still satisfied, new ones get extra features (the `read` query).

``` motoko no-repl
actor Counter_v2 {

  stable var state : Int = 0;

  public func inc() : async Int {
    state += 1;
    return state;
  };

  public query func read() : async Int { return state; }
}
```

|         |       |         |             |
|---------|-------|---------|-------------|
| version | state | success | call        |
| v1      | 3     | ✓       | inc()       |
| v1      | 4     | ✓       | upgrade(v2) |
| v2      | 4     | *✓*     | inc()       |
| v2      | 5     | ✓       | read()      |

## Changing the stable interface

Observation: the counter is always positive - let’s refactor `Int` to `Nat`!

``` motoko no-repl
actor Counter_v3 {

  stable var state : Nat = 0;

  public func inc() : async Nat {
    state += 1;
    return state;
  };

  public query func read() : async Nat { return state; }
}
```

|         |       |         |             |
|---------|-------|---------|-------------|
| version | state | success | call        |
| v2      | 5     | ✓       | inc()       |
| v2      | 6     | ✓       | upgrade(v3) |
| v3      | *0*   | *✗*     | inc()       |
| v3      | 1     | ✓       | read()      |

BOOM: code upgraded, but counter is back to `0`.

*The unthinkable has happened*: state was lost in an upgrade.

## What gives?

The Candid interface evolved safely …​ but the stable types did not.

An upgrade must be able to:

-   consume any stable variable value from its predecessor, or

-   run the initializer for a new stable variable.

Since `Int </: Nat`, the upgrade logic discards the saved `Int` (what if it was `-1`?) and re-runs the initializer instead.

What’s worse, the upgrade silently "succeeded", resetting the counter to `0`.

## Stable type signatures

A stable type signature looks like the "insides" of a Motoko actor type.

For example, `v2`'s stable types:

``` motoko no-repl
actor {
  stable var state : Int
};
```

An upgrade from `v2` to `v3`'s stable types:

``` motoko no-repl
actor {
  stable var state : Nat
};
```

requires consuming an `Int` as a `Nat`: a ***type error***.

## Dual interface evolution

An upgrade is safe provided:

-   the candid interface evolves to a subtype; and

-   the stable interface evolves to a compatible one (variable to supertype or new)

Given version `v0` with candid interface `v0.did` and stable type interface `v0.most`:

``` candid
service : {
  inc: () -> (int);
}
```

``` motoko no-repl
actor {

};
```

And version `v1` with candid interface `v1.did` and stable type interface `v1.most`,

``` candid
service : {
  inc: () -> (int);
}
```

``` motoko no-repl
actor {
  stable var state : Int
};
```

And version `v2` with candid interface `v2.did` and stable type interface `v2.most`,

``` candid
service : {
  inc: () -> (int);
  read: () -> (int) query;
}
```

``` motoko no-repl
actor {
  stable var state : Int
};
```

And, finally, version `v3` with candid interface `v3.did` and stable type interface `v3.most`:

``` candid
service : {
  inc: () -> (nat);
  read: () -> (nat) query;
}
```

``` motoko no-repl
actor {
  stable var state : Nat
};
```

The following table summarizes the (in)compatibilities between them:

|         |                  |                       |
|---------|------------------|-----------------------|
| version | candid interface | stable type interface |
| `v0`    | `v0.did`         | `v0.most`             |
|         | :> ✓             | \<\<: ✓               |
| `v1`    | `v1.did`         | `v1.most`             |
|         | :> ✓             | \<\<: ✓               |
| `v2`    | `v2.did`         | `v2.most`             |
|         | :> ✓             | \<\<: *✗*             |
| `v3`    | `v3.did`         | `v3.most`             |

## Tooling

Motoko compiler (`moc`) now supports:

-   `moc --stable-types …​` emits stable types to a `.most` file

-   `moc --stable-compatible <pre> <post>` checks two `.most` files for upgrade compatibility

To upgrade from `cur.wasm` to `nxt.wasm` we need check both Candid interface and stable variables are "compatible"

    didc check nxt.did cur.did  // nxt <: cur
    moc --stable-compatible cur.most nxt.most  // cur <<: nxt

E.g. the upgrade from `v2` to `v3` fails this check:

    > moc --stable-compatible v2.most v3.most
    (unknown location): Compatibility error [M0170], stable variable state of previous type
      var Int
    cannot be consumed at new type
      var Nat

## Examples in the wild

A common, real-world example of an incompatible upgrade can be found on the forum: <https://forum.dfinity.org/t/questions-about-data-structures-and-migrations/822/12?u=claudio>

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

Adding a new record field (to magic from nothing) does not work.

## Metadata Sections

Motoko embeds `.did` and `.most` files as wasm *custom sections*, for use by other tools, e.g. dfx.

In future, `dfx canister upgrade` will, by default:

1.  query the IC for a canister’s dual interfaces,

2.  check compatibility of the installed and new binary,

3.  abort the upgrade when unsafe.

## Why are we seeing data-loss only now?

A side-effect of a revision to Candid (used for stabilizing variables):

-   Previously, upgrades from `v2.wasm` to `v3.wasm` would fail and roll-back (no data loss).

-   Candid revision meant upgrade would now "succeed", but *with* data loss.

("fail safe" vs "silent failure")

## The right solution

What if we really do want to change `state` to `Nat`.

Solution: introduce a new stable variable, `newState`, initialized from the old one:

``` motoko no-repl
import Int "mo:base/Int";

actor Counter_v4 {

  stable var state : Int = 0;
  stable var newState : Nat = Int.abs(state);

  public func inc() : async Nat {
    newState += 1;
    return newState;
  };

  public query func read() : async Nat { return newState; }
}
```

``` motoko no-repl
actor {
  stable var newState : Nat;
  stable var state : Int
};
```

(Or use a variant from the start…​)
