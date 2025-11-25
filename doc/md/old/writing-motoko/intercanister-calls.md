---
sidebar_position: 12
---

# Inter-canister calls

One of the most important features of ICP for developers is the ability to call functions in one canister from another canister. This capability to make calls between canisters, also sometimes referred to as **inter-canister calls**, enables you to reuse and share functionality in multiple dapps.

For example, you might want to create a dapp for professional networking, organizing community events, or hosting fundraising activities. Each of these dapps might have a social component that enables users to identify social relationships based on some criteria or shared interest, such as friends and family or current and former colleagues.

To address this social component, you might want to create a single canister for storing user relationships then write your professional networking, community organizing, or fundraising application to import and call functions that are defined in the canister for social connections. You could then build additional applications to use the social connections canister or extend the features provided by the social connections canister to make it useful to an even broader community of other developers.

## Basic usage

A simple way to set up inter-canister calls is through your project's `dfx.json` file.

For example, let's say that you want to build a canister named `foo` which calls the canister `bar`.
Here is the `dfx.json` file:

```json
{
  "canisters": {
    "foo": {
      "dependencies": ["bar"],
      "type": "motoko",
      "main": "src/Foo.mo"
    },
    "bar": {
      "type": "motoko",
      "main": "src/Bar.mo"
    }
  }
}
```

Notice that `foo` includes `bar` as a canister dependency.

Below is an example implementation of `foo` (`src/Foo.mo`) which calls the `bar` canister:

```motoko no-repl
import Bar "canister:bar";

persistent actor Foo {

  public func main() : async Nat {
    let value = await Bar.getValue(); // Call a method on the `bar` canister
    value;
  };

};
```

Below is an implementation for `bar` (`src/Bar.mo`):

```motoko
import Debug "mo:core/Debug";

persistent actor Bar {

  public func getValue() : async Nat {
    Debug.print("Hello from the `bar` canister!");
    123;
  };

};
```

To run this example, you can use the `dfx canister call` subcommand (after deploying the canisters with `dfx deploy`):

```bash
dfx canister call foo main
```

The output should resemble the following:

```bash
2025-02-21 15:53:39.567801 UTC: [Canister ajuq4-ruaaa-aaaaa-qaaga-cai] Hello from the `bar` canister!
(123 : nat)
```

You can also use a canister ID to access a previously deployed canister as shown in this alternate implementation of `foo`:

```motoko
persistent actor Foo {
  public func main(canisterId: Text) : async Nat {
    let Bar = actor(canisterId): actor {
      getValue: () -> async Nat; // Define the expected canister interface
    };
    let value = await Bar.getValue(); // Call the canister
    value;
  };

};
```

Then, use the following call, replacing `canister-id` with the ID of a previously deployed canister:

```bash
dfx canister call foo main "canister-id"
```

## Advanced usage

If the method name or input types are unknown at compile time, it's possible to call arbitrary canister methods using the `InternetComputer` module.

Here is an example which you can modify for your specific use case:

```motoko
import IC "mo:core/InternetComputer";
import Debug "mo:core/Debug";

persistent actor AdvancedCanister1 {

  public func main(canisterId : Principal) : async Nat {
    // Define the method name and input args
    let name = "getValue";
    let args = (123);

    // Call the method
    let encodedArgs = to_candid (args);
    let encodedValue = await IC.call(canisterId, name, encodedArgs);

    // Decode the return value
    let ?value : ?Nat = from_candid encodedValue else Debug.trap("Unexpected return value");
    value;
  };

}
```

```motoko
import Debug "mo:core/Debug";

persistent actor AdvancedCanister2 {

  public func getValue(number: Nat) : async Nat {
     Debug.print("Hello from advanced canister 2!");
     return number * 2;
  };

};
```

In some situations, it may be useful to reference a canister by ID. This is possible with the following import syntax:

```motoko
import Canister "ic:7hfb6-caaaa-aaaar-qadga-cai";
```

If you do this, double check that the referenced canister is available and has the same canister ID in all intended runtime environments (usually the local replica and ICP mainnet).

In addition, a corresponding `.did` file with name `7hfb6-caaaa-aaaar-qadga-cai.did`, containing the Candid interface of the imported canister, must be available on the `moc` compiler's `actor-idl` path.

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />
