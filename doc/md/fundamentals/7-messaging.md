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

### Actor type annotation

Actor type annotations provide flexibility when interacting with external [canisters](https://internetcomputer.org/docs/building-apps/essentials/canisters) but require ensuring function signatures match at runtime. If they do not, calls will fail.

```motoko no-repl
import Array "mo:base/Array";

actor Publisher {
    stable var subscribers: [Principal] = [];

    public shared func subscribe(subscriber: Principal): async () {
        if (Array.find<Principal>(subscribers, func(s) { s == subscriber }) == null) {
            let newSubscribers = Array.tabulate<Principal>(
                subscribers.size() + 1,
                func(i) { if (i < subscribers.size()) subscribers[i] else subscriber }
            );
            subscribers := newSubscribers;
        };
    };

    public shared func publish(message: Text): async () {
        for (sub in subscribers) {
            let subActor = actor(sub) : actor { notify: (Text) -> async () };
            ignore await subActor.notify(message);
        };
    };
};
```

### Advanced inter-canister calls

When the method name or input types are unknown at compile time, the `ExperimentalInternetComputer` module can facilitate dynamic inter-canister communication.

```motoko no-repl
import IC "mo:base/ExperimentalInternetComputer";
import Debug "mo:base/Debug";

actor DynamicCaller {
    public shared func callMethod(canisterId: Principal, methodName: Text, arg: Nat): async Nat {
        let encodedArgs = to_candid(arg);
        let encodedResult = await IC.call(canisterId, methodName, encodedArgs);
        let ?result : ?Nat = from_candid encodedResult else Debug.trap("Invalid return");
        return result;
    };
};
```

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />