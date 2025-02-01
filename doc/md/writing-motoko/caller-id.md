---
sidebar_position: 6
---

# Caller identification



Motoko’s shared functions support a simple form of caller identification that allows you to inspect the ICP **principal** associated with the caller of a function. Principals are a value that identifies a unique user or canister.

You can use the principal associated with the caller of a function to implement a basic form of access control in your program.

## Using caller identification

In Motoko, the `shared` keyword is used to declare a shared function. The shared function can also declare an optional parameter of type `{caller : Principal}`.

To illustrate how to access the caller of a shared function, consider the following:

``` motoko
shared(msg) func inc() : async () {
  // ... msg.caller ...
}
```

In this example, the shared function `inc()` specifies a `msg` parameter, a record, and the `msg.caller` accesses the principal field of `msg`.

The calls to the `inc()` function do not change. At each call site, the caller’s principal is provided by the system, not the user. The principal cannot be forged or spoofed by a malicious user.

To access the caller of an actor class constructor, you use the same syntax on the actor class declaration. For example:

``` motoko
shared(msg) persistent actor class Counter(init : Nat) {
  // ... msg.caller ...
}
```

## Adding access control

To extend this example, assume you want to restrict the `Counter` actor so it can only be modified by the installer of the `Counter`. To do this, you can record the principal that installed the actor by binding it to an `owner` variable. You can then check that the caller of each method is equal to `owner` like this:

``` motoko file=../examples/Counters-caller.mo
```

In this example, the `assert (owner == msg.caller)` expression causes the functions `inc()` and `bump()` to trap if the call is unauthorized, preventing any modification of the `count` variable while the `read()` function permits any caller.

The argument to `shared` is just a pattern. You can rewrite the above to use pattern matching:

``` motoko file=../examples/Counters-caller-pat.mo
```

:::note

Simple actor declarations do not let you access their installer. If you need access to the installer of an actor, rewrite the actor declaration as a zero-argument actor class instead.

:::


## Recording principals

Principals support equality, ordering, and hashing, so you can efficiently store principals in containers for functions such as maintaining an allow or deny list. More operations on principals are available in the [principal](../base/Principal.md) base library.

The data type of `Principal` in Motoko is both sharable and stable, meaning you can compare `Principal`s for equality directly.

Below is an example of how you can record principals in a set.

``` motoko file=../examples/RecordPrincipals.mo
```


```motoko
import Principal "mo:base/Principal";
import OrderedSet "mo:base/OrderedSet";
import Error "mo:base/Error";

persistent actor {

    // Create set to store principals
    transient var principalSet = Set.Make(Principal.compare);

    var principals : OrderedSet.Set<Principal> = principalSet.empty();

    // Check if principal is recorded
    public shared query(msg) func isRecorded() : async Bool {
        let caller = msg.caller;
        principleSet.contains(principals, caller);
    };

    // Record a new principal
    public shared(msg) func recordPrincipal() : async () {
        let caller = msg.caller;
        if (Principal.isAnonymous(caller)) {
            throw Error.reject("Anonymous principal not allowed");
        };

        principals := principalSet.put(principals, caller)
    };
};
```

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />