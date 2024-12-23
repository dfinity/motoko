---
sidebar_position: 21
---

# Query functions



In ICP terminology, **update** messages, also referred to as calls, can alter the state of the canister when called. Effecting a state change requires agreement amongst the distributed replicas before the network can commit the change and return a result. Reaching consensus is an expensive process with relatively high latency.

For the parts of applications that don’t require the guarantees of consensus, the ICP supports more efficient query operations. These are able to read the state of a canister from a single replica, modify a snapshot during their execution and return a result, but cannot permanently alter the state or send further messages.

## Query functions

Motoko supports the implementation of queries using `query` functions. The `query` keyword modifies the declaration of a shared actor function so that it executes with non-committing and faster query semantics.

For example, consider the following `Counter` actor with a `read` function called `peek`:

``` motoko file=../examples/CounterWithQuery.mo
```

The `peek()` function might be used by a `Counter` frontend offering a quick, but less trustworthy, display of the current counter value.

Query functions can be called from non-query functions. Because those nested calls require consensus, the efficiency gains of nested query calls will be modest at best.

The `query` modifier is reflected in the type of a query function:

``` motoko no-repl
  peek : shared query () -> async Nat
```

As before, in `query` declarations and actor types the `shared` keyword can be omitted.

:::info

A query method cannot call an actor function and will result in an error when the code is compiled. Calls to ordinary functions are permitted.

:::


## Composite query functions

Queries are limited in what they can do. In particular, they cannot themselves issue further messages, including queries.

To address this limitation, the ICP supports another type of query function called a composite query.

Like plain queries, the state changes made by a composite query are transient, isolated and never committed. Moreover, composite queries cannot call update functions, including those
implicit in `async` expressions, which require update calls under the hood.

Unlike plain queries, composite queries can call query functions and composite query functions on the same and other actors, but only provided those actors reside on the same subnet.

As a contrived example, consider generalizing the previous `Counter` actor to a class of counters. Each instance of the class provides an additional `composite query` to sum the values of a given array of counters:

``` motoko file=../examples/CounterWithCompositeQuery.mo
```

Declaring `sum` as a `composite query` enables it call the `peek` queries of its argument counters.

While update messages can call plain query functions, they cannot call composite query functions. This distinction, which is dictated by the current capabilities of ICP, explains why query functions and composite query functions are regarded as distinct types of shared functions.

Note that the `composite query` modifier is reflected in the type of a composite query function:

``` motoko no-repl
  sum : shared composite query ([Counter]) -> async Nat
```

Since only a composite query can call another composite query, you may be wondering how any composite query gets called at all?

Composite queries are initiated outside ICP, typically by an application (such as a browser frontend) sending an ingress message invoking a composite query on a backend actor.

:::danger

The Internet Computer's semantics of composite queries ensures that state changes made by a composite query are isolated from other inter-canister calls, including recursive queries, to the same actor.

In particular, a composite query call rolls back its state on function exit, but is also does not pass state changes to sub-query or sub-composite-query calls. Repeated calls, which include recursive calls, have different semantics from calls that accumulate state changes.

In sequential calls, the internal state changes of preceding queries will have no effect on subsequent queries, nor will the queries observe any local state changes made by the enclosing composite query. Local states changes made by the composite query are preserved across the calls until finally being rolled-back on exit from the composite query.

This semantics can lead to surprising behavior for users accustomed to ordinary imperative programming.

Consider this example containing the composite query `test` that calls query `q` and composite query `cq`.


``` motoko no-repl file=../examples/CompositeSemantics.mo
```

When `state` is `0`, a call to `test` returns

```
{s0 = 0; s1 = 0; s2 = 0; s3 = 3_000}
```

This is because none of the local updates to `state` are visible to any of the callers or callees.

:::

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />