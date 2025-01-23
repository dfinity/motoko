---
sidebar_position: 28
---

# Candid serialization



Candid is an interface description language and serialization format designed specifically for the Internet Computer protocol.
It's a crucial component that enables seamless communication between different services and canister smart contracts on ICP, regardless of the programming languages they're implemented in.

At its core, Candid provides a language-agnostic way to describe and transmit data.
Strong typing guarantees accurate data interpretation across various services and languages.
This type safety is complemented by an efficient binary format for encoding data, making it ideal for network transmission.
In the context of Motoko, Candid is deeply integrated into the language.
Motoko automatically generates Candid interfaces for canister smart contracts and provides built-in functions like `to_candid` and `from_candid` for easy serialization and deserialization of data to and from Candid format.

In a broader scope, Candid serves as the standard communication protocol between canisters. When one canister calls another, the arguments are serialized to Candid, transmitted, and then deserialized by the receiving canister. This standardization enables developers to create frontends in languages like JavaScript that can easily interact with backend canisters written in Motoko or Rust.


Importantly, Candid's design allows for backwards-compatible upgrades of canister interfaces.
This feature facilitates the evolution of services over time, a critical aspect for long-lived applications
on the Internet Computer.


## Explicit Candid serialization

Motoko's `to_candid` and `from_candid` operators allow you to work with Candid-encoded data.

`to_candid (<exp1>, ..., <expn>)` serializes a sequence of Motoko values into a `Blob` containing a Candid binary encoding of the data.

For example,

``` motoko no-repl
let encoding : Blob = to_candid ("dogs", #are, ['g', 'r', 'e', 'a', 't']);
```

`from_candid <exp>` deserializes a blob that contains Candid data back into a Motoko value.

``` motoko no-repl
 let ?(t, v, cs) = from_candid encoding : ?(Text, {#are; #are_not}, [Char]);
```

`from_candid` will trap if its argument is a blob that does not contain valid Candid data.
Because deserialization can fail if the encoded value does not have the expected Candid type, `from_candid` returns a value of option type, with `null` indicating the encoding is well-formed but of the wrong Candid type or some value `?v`, where `v` is the decoded value. `from_candid` can only be used in the context of other code that determines its optional result type, for which a type annotation may be required.

For example, this code that under specifies the expected type of the decoded value is rejected by the compiler:

``` motoko no-repl
let ?(t, v, cs) = from_candid encoding;
```

The `to_candid` and `from_candid` operators are keywords built into the language and handle most common use cases automatically.
The operators ensure type safety and proper data encoding without requiring developers to manually handle the intricacies of Candid serialization.

:::danger

Although `to_candid` will return a valid Candid encoding of its argument, there are actually many different Candid encodings, and thus blobs, for the same value.
There is no guarantee that `to_candid` will always return the same `blob`, given the same argument.
That means that you should never use these blobs to compare values for equality or be tempted to
compute a hash for a value by hashing its Candid encoding.
The hash of a value should be unique, but if you compute it from one of several Candid encodings, it may not be.

:::

See the language manual for more details on [`Candid serialization`](../reference/language-manual#candid_serialization).


## Dynamic calls

Most users should never need to use `to_candid` and `from_candid`.
One scenario in which the operations are useful is when calling canister methods dynamically using the `call` function from the `ExperimentalInternetComputer` base library.

Although most canisters on ICP speak Candid, this isn't mandated by ICP. At the protocol level, canisters communicate in raw binary data. Candid is just a common interpretation of that data that allows canisters written in different languages to interoperate.

The `call` function takes a canister principal, the name of a method as text, and a raw binary blob and returns a future containing the result of the call, also as a raw binary blob.

Dynamic calls are particularly useful when working with canisters or services that have complex or non-standard interfaces, or when you need fine-grained control over the calling process. However, they require manual handling of binary encoding and decoding, which is more error-prone than using the high-level abstractions provided by Motoko.

When a service does speak Candid and you know the types of the method you want to invoke, you can use `to_candid` and `from_candid` to deal with the binary format.

Typically, you might use `to_candid` to prepare the argument of a call and `from_candid` to process its result.

In this example, we use the imported `call` function to make a dynamic call on the actor:

``` motoko no-repl
import Principal "mo:base/Principal";
import {call} "mo:base/ExperimentalInternetComputer";

persistent actor This {

   public func concat(ts : [Text]) : async Text {
      var r = "";
      for (t in ts.vals()) { r #= t };
      r
   };

   public func test() : async Text {
       let arguments = to_candid (["a", "b", "c"]);
       let results = await call(Principal.fromActor(This), "concat", arguments);
       let ?t = from_candid(results) : ?Text;
       t
   }

}
```

While dynamic calls offer more flexibility, they should be used judiciously.
In most cases, the standard inter-canister call mechanisms and automatic Candid handling in Motoko provide a safer and more convenient approach to canister interactions.


## Resources

For further information on Candid, check out the documentation here:

- [Candid UI](/docs/current/developer-docs/smart-contracts/candid).

- [What is Candid?](/docs/current/developer-docs/smart-contracts/candid/candid-concepts).

- [Using Candid](/docs/current/developer-docs/smart-contracts/candid/candid-howto).

- [Candid specification](https://github.com/dfinity/candid/blob/master/spec/Candid.md).

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />
