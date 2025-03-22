---
sidebar_position: 3
---


# Actor classes



Actor classes enable you to create networks of actors programmatically. Actor classes have to be defined in a separate source file. To illustrate how to define and import actor classes, the following example implements a distributed map of keys of type [`Nat`](../base/Nat.md) to values of type [`Text`](../base/Text.md). It provides simple insert and lookup functions, `put(k, v)` and `get(k)`, for working with these keys and values.

## Defining an actor class

To distribute the data for this example, the set of keys is partitioned into `n` buckets. For now, we just fix `n = 8`. The bucket, `i`, of a key, `k`, is determined by the remainder of `k` divided by `n`, that is, `i = k % n`. The `i`th bucket (`i` in `[0..n)`) receives a dedicated actor to store text values assigned to keys in that bucket.

The actor responsible for bucket `i` is obtained as an instance of the actor class `Bucket(i)`, defined in the sample `Buckets.mo` file, as follows:

`Buckets.mo`:

``` motoko no-repl name=Buckets file=../examples/Buckets.mo
```

A bucket stores the current mapping of keys to values in a mutable `map` variable containing an imperative RedBlack tree, `map`, that is initially empty.

On `get(k)`, the bucket actor simply returns any value stored at `k`, returning `map.get(k)`.

On `put(k, v)`, the bucket actor updates the current `map` to map `k` to `?v` by calling `map.put(k, v)`.

Both functions use the class parameters `n` and `i` to verify that the key is appropriate for the bucket by asserting `((k % n) == i)`.

Clients of the map can then communicate with a coordinating `Map` actor, implemented as follows:

``` motoko no-repl include=Buckets file=../examples/Map.mo
```

As this example illustrates, the `Map` code imports the `Bucket` actor class as module `Buckets`.

The actor maintains an array of `n` allocated buckets, with all entries initially `null`. Entries are populated with `Bucket` actors on demand.

On `get(k, v)`, the `Map` actor:

-   Uses the remainder of key `k` divided by `n` to determine the index `i` of the bucket responsible for that key.

-   Returns `null` if the `i`th bucket does not exist, or

-   Delegates to that bucket by calling `bucket.get(k, v)` if it does.

On `put(k, v)`, the `Map` actor:

-   Uses the remainder of key `k` divided by `n` to determine the index `i` of the bucket responsible for that key.

-   Installs bucket `i` if the bucket does not exist by using an asynchronous call to the constructor, `Buckets.Bucket(i)`, and, after awaiting the result, records it in the array `buckets`.

-   Delegates the insertion to that bucket by calling `bucket.put(k, v)`.

While this example sets the number of buckets to `8`, you can generalize the example by making the `Map` actor an actor class, adding a parameter `(n : Nat)` and omitting the declaration `let n = 8;`.

For example:

``` motoko no-repl
actor class Map(n : Nat) {

  type Key = Nat
  ...
}
```

Clients of actor class `Map` are now free to determine the maximum number of buckets in the network by passing an argument on construction.

:::note

On ICP, calls to a class constructor must be provisioned with cycles to pay for the creation of a principal. See [ExperimentalCycles](../base/ExperimentalCycles.md) for instructions on how to add cycles to a call using the imperative `ExperimentalCycles.add<system>(cycles)` function.

:::

## Configuring and managing actor class instances

On ICP, the primary constructor of an imported actor class always creates a new principal and installs a fresh instance of the class as the code for that principal.

To provide further control over actor class installation, Motoko endows each imported actor class with an extra, secondary constructor. This constructor takes an additional first argument that specifies the desired installation mode. The constructor is only available via special syntax that stresses its `system` functionality.

Using this syntax, it's possible to specify initial canister settings (such as an array of controllers), manually install, upgrade and reinstall canisters, exposing all of the
lower-level facilities of the Internet Computer.

See [actor class management](../reference/language-manual#actor-class-management) for more details.

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />