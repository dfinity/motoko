---
sidebar_position: 4
---

# Candid serialization

[Candid](https://internetcomputer.org/docs/building-apps/interact-with-canisters/candid/candid-concepts) is an interface description language and serialization format designed specifically for the Internet Computer. It enables different services and canisters to communicate regardless of their implementation languages, as Candid provides a [language-agnostic way](https://internetcomputer.org/docs/building-apps/interact-with-canisters/candid/using-candid) to describe and transmit data. It uses [strong typing](https://internetcomputer.org/docs/references/candid-ref) that guarantees accurate data interpretation across services and languages. This type safety is complemented by an efficient binary format for encoding data, making it ideal for network transmission.

[Candid](https://internetcomputer.org/docs/building-apps/interact-with-canisters/candid/candid-concepts) serves as the standard communication protocol between canisters. **Motoko canisters automatically generate Candid interfaces.** Motoko also provides built-in functions for easy serialization and deserialization of data to and from Candid. When one canister calls another, the arguments are [serialized to Candid]((https://internetcomputer.org/docs/building-apps/interact-with-canisters/candid/using-candid)), transmitted, and then deserialized by the receiving canister. This standardization enables developers to create frontends in languages like JavaScript that can easily interact with backend canisters written in Motoko or other languages.

Candid's design allows for backwards-compatible upgrades of canister interfaces, facilitating the evolution of services over time. 

## Working with Candid

The Motoko compiler generates a [Candid interface (`.did`) file](https://internetcomputer.org/docs/building-apps/interact-with-canisters/candid/using-candid#the-did-file) corresponding to a canister's public methods. This file ensures that data passed into and returned from these methods is automatically encoded to and decoded from Candid’s binary format. 

Motoko provides the `to_candid` and `from_candid` functions for serializing and deserializing Candid-encoded data. The `to_candid` function serializes a sequence of Motoko values into a `Blob` containing a Candid binary encoding of the data. Motoko canisters automatically handle the serialization and deserialization of data when the canister interacts with [inter-canister calls or ingress messages](https://internetcomputer.org/docs/building-apps/essentials/message-execution).

There are many different Candid encodings, and thus `Blob`s, for the same value. There is no guarantee that `to_candid` will always return the same `Blob` given the same argument. That means that you should never use them to compare values for equality or compute a hash for a value using its Candid encoding. 

Typically, explicit serialization using `to_candid` and `from_candid` isn't necessary in everyday development and is mainly reserved for advanced scenarios, such as dynamic calls to arbitrary canisters, handling binary data for storage, or managing canister upgrades where you explicitly control data encoding. 
### `to_candid`

The `to_candid` serializes Motoko values into a Candid-encoded binary `Blob`. This is useful when explicitly encoding data for storage, transmission between canisters, or dynamic inter-canister calls. The `to_candid` function can accept one or multiple arguments separated by commas:

```motoko no-repl
let encoding =  to_candid(true,"hello", 68,-90) //Bool, Text, Nat, Int
```

Each argument must be composed of sharable types. The resulting `Blob` precisely represents the original Motoko values according to Candid specifications.

```motoko no-repl
import Debug "mo:base/Debug";

actor {
  public type User = {
    userId : Nat;
    name : Text;
  };
  
    public func serializeUser(user: User) : async Blob {
        let encodedData : Blob = to_candid(user);
        
        Debug.print("User data serialized successfully.");

        return encodedData;
    };
}
```

### `from_candid`

The `from_candid` function deserializes a Candid-encoded binary `Blob` back into Motoko values. This explicit decoding can be useful when retrieving serialized data for processing or state restoration.

`from_candid` requires clear type context or annotations to decode the `Blob` correctly. The result is wrapped in an optional (`?`) type to handle cases where decoding might fail due to type mismatch or invalid data.

```motoko no-repl
import Debug "mo:base/Debug";

actor {
  public type User = {
    userId : Nat;
    name : Text;
  };

  public func deserializeUser(encodedData : Blob) : async ?User {
    let decodedUser : ?User = from_candid(encodedData);

    switch decodedUser {
      case (?user) {
        Debug.print("User data deserialized successfully.");
        return ?user;
      };
      case null {
        Debug.print("Deserialization failed: Invalid blob or type mismatch.");
        return null;
      };
    };
  };
}
```

## Dynamic calls

Although most canisters on ICP utilize Candid, the protocol itself doesn't mandate it. At the [protocol level]https://learn.internetcomputer.org/hc/en-us/articles/34206453538964-Blockchain-Protocol, canisters communicate using raw binary data. Candid serves as a widely adopted standard to interpret this binary data.

Explicit handling of Candid encoding may be beneficial when making dynamic calls to canister methods using the `call` function from the `ExperimentalInternetComputer` module.

The `call` function requires three parameters:

1. A canister's principal.
2. The method name as a `Text`.
3. Raw binary data (`Blob`) as an input argument. 

It returns a future (`async`) containing the method's result as raw binary data.

Dynamic calls are especially useful when interacting with canisters that have complex or non-standard interfaces, or when developers require granular control over the calling process. However, they require that you manually handle the binary encoding and decoding, which can introduce errors.

If the target service uses Candid and the method's types are known, developers can conveniently leverage `to_candid` and `from_candid` for binary data handling.

Here's an example demonstrating how to perform a dynamic call:

```motoko no-repl
import Principal "mo:base/Principal";
import { call } "mo:base/ExperimentalInternetComputer";
import Debug "mo:base/Debug";

actor This{
  public type User = {
    userId : Nat;
    name : Text;
  };

  public func createUser(userId : Nat, name : Text) : async User {
    { userId = userId; name = name }
  };

  // Dynamically call 'createUser' method on the same actor
  public func dynamicCreateUser(userId : Nat, name : Text) : async ?User {
    let args = to_candid(userId, name); // encode arguments explicitly
    let resultBlob = await call(Principal.fromActor(This), "createUser", args);

    let decodedUser : ?User = from_candid(resultBlob);
    switch decodedUser {
      case (?user) {
        Debug.print("User created dynamically: " # user.name);
        return ?user;
      };
      case null {
        Debug.print("Failed to decode dynamic call result.");
        return null;
      };
    };
  };
}
```

Dynamic calls should be used thoughtfully. In most cases, standard inter-canister calls and automatic Candid handling provided by Motoko offer a safer, simpler, and more convenient approach.

### Resources

For more detailed information on Candid, refer to:

- [Candid UI](https://internetcomputer.org/docs/current/developer-docs/integrations/candid-ui/)
- [What is Candid?](https://internetcomputer.org/docs/current/developer-docs/build/candid/)
- [Using Candid](https://internetcomputer.org/docs/current/developer-docs/build/candid/how-to-use-candid/)
- [Candid Specification](https://internetcomputer.org/docs/current/references/candid-spec/)
