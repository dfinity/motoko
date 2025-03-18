---
sidebar_position: 4
---

# Candid serialization

**Candid** is an interface description language and serialization format designed specifically for the Internet Computer protocol. It enables communication between different services and canisters on ICP, regardless of their implementation languages. At its core, Candid provides a language-agnostic way to describe and transmit data with strong typing that guarantees accurate data interpretation across various services and languages. This type safety is complemented by an efficient binary format for encoding data, making it ideal for network transmission.

In the context of Motoko, Candid is deeply integrated into the language. **Motoko automatically generates Candid interfaces** for canisters and provides built-in functions for easy serialization and deserialization of data to and from Candid format. In a broader scope, **Candid serves as the standard communication protocol between canisters**. When one canister calls another, the arguments are serialized to Candid, transmitted, and then deserialized by the receiving canister. This standardization enables developers to create frontends in languages like JavaScript that can easily interact with backend canisters written in Motoko or Rust.

Importantly, Candid's design allows for backwards-compatible upgrades of canister interfaces. This feature facilitates the evolution of services over time, a critical aspect for long-lived applications on the Internet Computer.

## Working with Candid

Motoko provides the `to_candid` and `from_candid` functions for serializing and deserializing Candid-encoded data. The `to_candid` function serializes a sequence of Motoko values into a Blob containing a Candid binary encoding of the data. Typically, explicit serialization using `to_candid` and `from_candid` isn't necessary in everyday development. On the Internet Computer, Motoko automatically handles the serialization and deserialization of data when canisters interact through normal inter-canister calls or when clients interact with canisters.

When defining public methods in a Motoko canister, the compiler generates a corresponding Candid interface (`.did` file). This ensures that data passed into and returned from these methods is automatically encoded to and decoded from Candid’s binary format. Explicit serialization with `to_candid` and `from_candid` is mainly reserved for advanced scenarios, such as dynamic calls to arbitrary canisters, handling binary data for storage, or managing canister upgrades where you explicitly control data encoding.

Although `to_candid` will return a valid Candid encoding of its argument, there are actually many different Candid encodings, and thus blobs, for the same value. There is no guarantee that `to_candid` will always return the same blob, given the same argument. That means that you should never use these blobs to compare values for equality or be tempted to compute a hash for a value by hashing its Candid encoding. The hash of a value should be unique, but if you compute it from one of several Candid encodings, it may not be.

### `to_candid`

The `to_candid` function in Motoko serializes Motoko values into a Candid-encoded binary `Blob`. This serialization is useful when explicitly encoding data for storage, transmission between canisters, or dynamic inter-canister calls. The to_candid function can accept no or multiple arguments separated by commas e.g:

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

The `from_candid` function in Motoko deserializes a Candid-encoded binary `Blob` back into Motoko values. This explicit decoding can be useful when retrieving previously serialized data for processing or state restoration.

`from_candid` requires clear type context or annotations to decode the blob correctly. The result is wrapped in an optional (`?`) type, handling cases where decoding might fail due to type mismatch or invalid data.

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

Explicit serialization using `to_candid` and `from_candid` is typically unnecessary for routine development. One scenario where explicit handling of Candid encoding is beneficial is when making dynamic calls to canister methods using the `call` function from the `ExperimentalInternetComputer` module.

Although most canisters on the Internet Computer (ICP) utilize Candid, the ICP protocol itself doesn't mandate it. At the protocol level, canisters communicate using raw binary data. Candid serves as a widely adopted standard to interpret this binary data, enabling interoperability among canisters written in various languages.

The `call` function requires three parameters: a canister's principal, the method name as a `Text`, and raw binary data (`Blob`) as input arguments. It returns a future (`async`) containing the method's result, also as raw binary data.

Dynamic calls are especially useful when interacting with canisters with complex or non-standard interfaces, or when developers require granular control over the calling process. However, they necessitate manual handling of binary encoding and decoding, which can introduce errors compared to Motoko’s automatic handling.

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

Dynamic calls provide flexibility, but they should be used thoughtfully. In most cases, standard inter-canister calls and automatic Candid handling provided by Motoko offer a safer, simpler, and more convenient approach for interactions.

### Resources

For more detailed information on Candid, refer to:

- [Candid UI](https://internetcomputer.org/docs/current/developer-docs/integrations/candid-ui/)
- [What is Candid?](https://internetcomputer.org/docs/current/developer-docs/build/candid/)
- [Using Candid](https://internetcomputer.org/docs/current/developer-docs/build/candid/how-to-use-candid/)
- [Candid Specification](https://internetcomputer.org/docs/current/references/candid-spec/)
