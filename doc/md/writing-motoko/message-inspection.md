---
sidebar_position: 14
---

# Message inspection



On ICP, a canister can selectively inspect, then choose to accept or decline ingress messages submitted through the HTTP interface.

> A canister can inspect ingress messages before executing them. When the IC receives an update call from a user, the IC will use the canister method `canister_inspect_message` to determine whether the message shall be accepted. If the canister is empty (i.e. does not have a Wasm module), then the ingress message will be rejected. If the canister is not empty and does not implement `canister_inspect_message`, then the ingress message will be accepted.
>
> In `canister_inspect_message`, the canister can accept the message by invoking `ic0.accept_message : () → ()`. This function traps if invoked twice. If the canister traps in `canister_inspect_message` or does not call `ic0.accept_message`, then the access is denied.
>
> The `canister_inspect_message` is not invoked for HTTP query calls, inter-canister calls or calls to the management canister.
>
> —  [IC Interface Specification](https://internetcomputer.org/docs/current/references/ic-interface-spec/#system-api-inspect-message)

Message inspection mitigates some denial of service attacks that are designed to drain canisters of cycles by placing unsolicited free calls.

## Message inspection in Motoko

In Motoko, actors can elect to inspect and accept or decline ingress messages by declaring a particular `system` function called `inspect`. Given a record of message attributes, this function produces a [`Bool`](../base/Bool.md) that indicates whether to accept or decline the message by returning `true` or `false`.

The function is invoked by the system on each ingress message. Similar to a query, any side effects of an invocation are discarded and transient. A call that traps due to some fault has the same result as returning a message declination of `false`.

Unlike other system functions that have a fixed argument type, the argument type of `inspect` depends on the interface of the enclosing actor. In particular, the formal argument of `inspect` is a record of fields with the following types:

-   `caller : Principal`: The principal of the caller of the message.

-   `arg : Blob`: The raw, binary content of the message argument.

-   `msg : <variant>`: A variant of decoding functions, where `<variant> == {…​; #<id>: () → T; …​}` contains one variant per shared function, `<id>`, of the actor. The variant’s tag identifies the function to be called. The variant’s argument is a function that returns the decoded argument of the call as a value of type `T`.

Using a variant, tagged with `#<id>`, allows the return type, `T`, of the decoding function to vary with the argument type, also `T`, of the shared function `<id>`.

The variant’s argument is a function so that one can avoid the expense of message decoding.

By exploiting subtyping, the formal argument can omit record fields it does not require, or selectively ignore the arguments of particular shared functions. For example, to simply dispatch on the name of a function without inspecting its actual argument.

:::note

A `shared query` function can be called using a regular HTTP update call to obtain a certified response. This is why the variant type also includes `shared query` functions.

A `shared composite query` function cannot be called as an update call. It can only be called with the faster, but uncertified, HTTP query call.

This is why the `inspect` variant type includes `shared query` functions, but not `shared composite query` functions.

:::

:::danger

An actor that fails to declare system field `inspect` will simply accept all ingress messages.

:::

:::danger

System function `inspect` should **not** be used for definitive access control. This is because `inspect` is executed by a single replica, without going through full consensus. Its result could be spoofed by a malicious boundary node. Also `inspect` is not invoked for inter-canister calls. Reliable access control checks can only be performed within the `shared` functions guarded by `inspect`. See [canister development security best practices](https://internetcomputer.org/docs/current/developer-docs/security/rust-canister-development-security-best-practices#do-not-rely-on-ingress-message-inspection) for more information.

:::

## Example

A simple example of method inspection is a counter actor that inspects some of its messages in detail, and others only superficially:

``` motoko file=../examples/InspectFull.mo
```

Due to subtyping, all of the following variations in order of increasing argument specificity, are legal definitions of `inspect`.

Blanket denial of all ingress messages, ignoring further information:

``` motoko no-repl file=../examples/InspectNone.mo#L10-L10
```

Declining anonymous calls:

``` motoko no-repl file=../examples/InspectCaller.mo#L12-L14
```

Declining large messages, based on the raw size in bytes of `arg` prior to any decoding from Candid binary blob to Motoko value:

``` motoko no-repl file=../examples/InspectArg.mo#L10-L13
```

Declining messages by name only, ignoring message arguments. Note the use of type `Any` as message argument variants:

``` motoko no-repl file=../examples/InspectName.mo#L10-L23
```

A combination of the previous three, specifying the argument types of some variants while ignoring others at type `Any` and using pattern matching to conflate identical cases:

``` motoko no-repl file=../examples/InspectMixed.mo#L12-L30
```

## Tips on authoring `inspect`

Implementing `inspect` after the fact once all shared functions of an actor have already been implemented can be tedious. You’ll need to declare a correctly typed variant for each shared function. A simple trick is to first implement the function incorrectly with a `()` argument, compile the code, then use the compiler’s error message to obtain the required argument type.

For example, in the actor from the previous section, incorrectly declaring forces the compiler to report the expected type below, which you can then cut-and-paste into your code:

``` motoko no-repl file=../examples/InspectTrick.mo#L11-L13
```

``` motoko no-repl
Inspect.mo:12.4-14.5: type error [M0127], system function inspect is declared with type
  () -> Bool
instead of expected type
  {
    arg : Blob;
    caller : Principal;
    msg :
      {
        #inc : () -> ();
        #read : () -> ();
        #reset : () -> ();
        #set : () -> Nat
      }
  } -> Bool
```

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />