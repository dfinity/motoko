---
sidebar_position: 7
---

# Messaging

## Messaging restrictions

ICP enforces rules on when and how [canisters](https://internetcomputer.org/docs/building-apps/essentials/canisters) communicate. These restrictions prevent execution errors statically in Motoko.

| Restriction | Reason |
|-------------|--------|
| A canister cannot send messages during installation. | Prevents execution errors at deployment. |
| A query function cannot send messages. | Query functions run locally and do not trigger updates. |
| Shared functions cannot be called in a synchronous context. | Shared calls require `async` execution. |
| Only async contexts support [error handling](https://internetcomputer.org/docs/motoko/fundamentals/error-handling) (`try/catch`). | Messaging errors only occur in async contexts. |

In Motoko, an expression is in an async context if it appears in an `async` function. Query functions are 'ready only' functions, so they do not create an async context and cannot use `await` or send messages.

```motoko no-repl
actor Counter {
    stable var count: Nat = 0;

    public shared func increment(): async () {
        count += 1;
    };

    public query func getCount(): async Nat {
        return count;  // Allowed: No state change
    };

    public func invalidCall() : Nat {
        return await getCount();  // Error: Query function cannot be awaited in a sync function
    };
};
```

## Message inspection

Canisters can inspect and filter incoming messages before execution to prevent spam or malicious requests. This is done using the `inspect` system function, which can accept or reject messages based on criteria like caller identity, message size, or function type. Messages are filtered before execution, preventing unnecessary cycles costs. Anonymous calls can be blocked to prevent spam, and the size of a request can be checked before processing.

```motoko no-repl
import Principal "mo:base/Principal";

actor Counter {
    stable var count: Nat = 0;

    public shared func increment(): async () { count += 1; };
    public shared func reset(): async () { count := 0; };
    public query func getCount(): async Nat { count; };

    system func inspect({ caller: Principal; arg: Blob; msg: { #increment: (); #reset: (); #getCount: () } }) : Bool {
        if (Principal.isAnonymous(caller)) return false;  // Reject anonymous calls
        if (arg.size() > 512) return false;  // Reject large requests
        switch (msg) {
            case (#increment _) { true };  // Allow increment
            case (#reset _) { false };  // Reject reset calls
            case (#getCount _) { true };  // Allow getCount
        }
    };
};
```

## Inter-canister calls

One of the key features of ICP is the ability for [canisters](https://internetcomputer.org/docs/building-apps/essentials/canisters) to invoke functions in other canisters. This capability, known as inter-canister calls, allows functionality to be reused and shared across multiple dapps.

The following methods can be used for inter-canister calls:

| Method | Usage scenario | Example |
|--------|--------------|---------|
| Direct import | When the canister is part of the same project and explicitly imported. | `import Subscriber "canister:subscriber";` |
| Actor type annotation | When calling an external canister that is part of the project but deployed separately. | `let sub = actor(canisterId) : actor { notify: (Text) -> async () };` |
| Dynamic calls | When calling unknown functions or passing dynamic arguments. | `await IC.call(canisterId, methodName, encodedArgs);` |

### Canister imports

When a canister exists in the project directory, it can be imported using the `import` statement. This ensures strong typing and allows safe function calls.

In this example, a publisher canister maintains a list of subscribers and sends notifications when an event occurs. Each subscriber canister receives and processes notifications.

#### Publisher

```motoko no-repl
import Subscriber "canister:subscriber";
import Array "mo:base/Array";

actor Publisher {
    stable var subscribers: [Subscriber.Subscriber] = [];

    public shared func subscribe(subscriber: Subscriber.Subscriber): async () {
        if (Array.find<Subscriber.Subscriber>(subscribers, func(s) { s == subscriber }) == null) {
            let newSubscribers = Array.tabulate<Subscriber.Subscriber>(
                subscribers.size() + 1,
                func(i) { if (i < subscribers.size()) subscribers[i] else subscriber }
            );
            subscribers := newSubscribers;
        };
    };

    public shared func publish(message: Text): async () {
        for (sub in subscribers) {
            ignore await sub.notify(message);
        };
    };
};

```

#### Subscriber

```motoko no-repl
import Debug "mo:base/Debug";

actor Subscriber {
    public shared func notify(message: Text): async () {
        Debug.print("Received message: " # message);
    };
};

```

### Actor type annotations

Actor type annotations provide flexibility when interacting with external [canisters](https://internetcomputer.org/docs/building-apps/essentials/canisters) but require ensuring function signatures match at runtime. If they do not, calls will fail.

Actor interfaces are declared using a `module`, then applied using a type annotation when calling an actor by its principal.

Consider the following actor type annotation when interacting with the [management canister](https://internetcomputer.org/docs/references/system-canisters/management-canister/) to check the status of a canister.

```motoko no-repl
// ic.mo
module {
  public type canister_id = Principal;

  public type definite_canister_settings = {
    controllers : [Principal];
    compute_allocation : Nat;
    memory_allocation : Nat;
    freezing_threshold : Nat;
    reserved_cycles_limit : Nat;
    log_visibility : {
      #controllers;
      #public;
      #allowed_viewers : [Principal];
    };
    wasm_memory_limit : Nat;
    wasm_memory_threshold : Nat;
  };

  public type canister_status_args = {
    canister_id : canister_id;
  };

  public type canister_status_result = {
    status : { #running; #stopping; #stopped };
    settings : definite_canister_settings;
    module_hash : ?Blob;
    memory_size : Nat;
    memory_metrics : {
      wasm_memory_size : Nat;
      stable_memory_size : Nat;
      global_memory_size : Nat;
      wasm_binary_size : Nat;
      custom_sections_size : Nat;
      canister_history_size : Nat;
      wasm_chunk_store_size : Nat;
      snapshots_size : Nat;
    };
    cycles : Nat;
    reserved_cycles : Nat;
    idle_cycles_burned_per_day : Nat;
    query_stats: {
      num_calls_total: Nat;
      num_instructions_total: Nat;
      request_payload_bytes_total: Nat;
      response_payload_bytes_total: Nat;
    };
  };

  public type Self = actor {
    canister_status : shared query canister_status_args -> async canister_status_result;
  };
};
```

To use the interface, instantiate the actor using its principal and call the desired method defined in the annotated type.

```motoko no-repl
// checker.mo
import IC "ic.mo";

actor StatusChecker {
  public shared func getStatus(canisterId: Principal): async IC.canister_status_result {
    let ic : IC.Self = actor "aaaaa-aa";
    let result = await ic.canister_status({ canister_id = canisterId });
    return result;
  };
};
```

::::note Considerations

:::note [Caller authorization]
To query a canister’s status using `canister_status`, the caller must be a **controller** of that canister. Otherwise, the call will be rejected with an authorization error.
:::

:::note [API changes]
The interface of system canisters—like the Management Canister—can evolve over time. Always refer to the [IC interface spec](https://github.com/dfinity/portal/blob/39cee0c2f44ba3b39c5c935ee4de7b36ea7b69ac/docs/references/_attachments/ic.did#L4) to ensure your type annotations remain accurate.
:::

:::tip [Extending the interface]
This interface defines the types and arguments required to call the `canister_status()` function.
To call additional functions exposed by the actor, simply extend the `Self` type with the corresponding function signatures.
:::

::::

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />