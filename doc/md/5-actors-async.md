---
sidebar_position: 5
---

# Actors & async data

The actor model, introduced by Carl Hewitt in the 1970s, was designed to solve concurrency issues by encapsulating [state](https://internetcomputer.org/docs/motoko/fundamentals/state) and computation within independent units called **actors**.

The actor model is based on the following principles:

- Actors are isolated from each other and communicate only by exchanging messages.
- Actors can receive and process messages concurrently.
- Actors are independent of each other and can fail independently.
- Actors are location transparent, which means that an actor can be located on any machine in a distributed system.

Motoko adopts the actor model to enable scalable, non-blocking execution on the Internet Computer. Motoko actors run in a single-threaded environment but achieve high concurrency by handling requests [asynchronously](https://internetcomputer.org/docs/motoko/fundamentals/actors-async#async--await).

## Async actors

To demonstrate how asynchronous actors work, consider the following example.

Customers place orders at a pizza restaurant, but the chef can only make one pizza at a time. Orders are taken **[asynchronously](https://internetcomputer.org/docs/motoko/fundamentals/actors-async#async--await)**, meaning customers do not have to wait for previous orders to be completed before placing their own. However, each pizza is prepared sequentially. This is representative of an asynchronous actor.

```motoko no-repl
import Array "mo:base/Array";
import Text "mo:base/Text";

persistent actor PizzaParlor {
    var orders : [Text] = [];

    public func placeOrder(order : Text) : async Text {
        // Use Array.tabulate to create a new array with the additional element
        let newOrders = Array.tabulate<Text>(orders.size() + 1, func(i) {
            if (i < orders.size()) { orders[i] } else { order }
        });
        orders := newOrders;
        return "Order received: " # order;
    };

    public func makePizza() : async Text {
        if (orders.size() == 0) {
            return "No orders to make.";
        };

        let currentOrder = orders[0];
        // Use Array.filter to remove the first order, assuming orders are unique
        orders := Array.filter<Text>(orders, func(o) {
            not Text.equal(o, currentOrder)
        });
        return "Made a delicious " # currentOrder # " pizza!";
    };

    public query func getOrders() : async [Text] {
        return orders;
    };
}
```

## `async` / `await`

The `await` keyword suspends the execution of the current function until the asynchronous operation completes, allowing the actor to continue processing other messages.

```motoko no-repl
await PizzaParlor.placeOrder("BBQ"); // Order received: BBQ
```

This request is handled [asynchronously](https://internetcomputer.org/docs/motoko/fundamentals/actors-async#async--await), meaning other orders can be placed without waiting for a response. The actor remains responsive, even while executing other tasks.

## `async*` / `await*`

In other languages, `async` gives a future and `async*` gives a stream that can yield values as defined by the stream type. Motoko does not do the latter. Instead, `async*` is used for internal processes that may be `async`. The `await*` expression works by evaluating its expression to a result. If that result is a delayed computation, it executes that computation synchronously first. It only switches to asynchronous execution when (and if) it encounters a regular `await` inside that computation. This means `await*` doesn't automatically commit state changes or suspend execution like regular `await` does. `await*` does not invoke messaging unless it is forced.

Customers can place multiple orders at the same time. However, when they ask for an update on their delivery status, responses must be generated sequentially based on the preparation time. Instead of constantly tracking delivery updates, `async*` can be used to calculate the estimated delivery time only when requested.

```motoko no-repl
// async* function to calculate delivery time on demand
func checkDelivery(order : Text) : async* Text {
        let estimatedTime = 15 + (orders.size() * 5); // Base time + 5 mins per pending order
        return "Your " # order # " will arrive in " # Nat.toText(estimatedTime) # " minutes.";
    };

    // Multiple users can check their delivery status asynchronously
    public shared func getDeliveryStatus(order : Text) : async Text {
        let status = await* checkDelivery(order);
        return status;
    };
```

A customer places an order:

```motoko no-repl
let response = await pizzaParlor.placeOrder("pepperoni");
// "Order received: pepperoni"
```

Another customer asks for a delivery update:

```motoko no-repl
let status = await pizzaParlor.getDeliveryStatus("pepperoni");
// "Your pepperoni will arrive in 20 minutes."
```

Another customer asks immediately after:

```motoko no-repl
let status2 = await pizzaParlor.getDeliveryStatus("margherita");
// "Your margherita will arrive in 25 minutes."
```

## `await` vs `await*`

| Feature         | `await`                      | `await*`                        |
|---------------|-----------------------------|--------------------------------|
| Execution     | Suspends execution until the result is available. | Does not suspend execution unless an `await` exists inside `async*`. |
| State commit | Commits all [state](https://internetcomputer.org/docs/motoko/fundamentals/state) changes before resuming. | Does not commit [state](https://internetcomputer.org/docs/motoko/fundamentals/state) unless a proper `await` is inside. |
| Re-evaluation | Runs once, returning the same value if awaited multiple times. | Each `await*` triggers a fresh computation of `async*`. |

### `await` example

```motoko 
let x : async Nat = read();
let result = await x; // State is committed here, execution suspends
```

### `await*` example

```motoko
let x : async* Nat = compute();
let result = await* x; // No state commitment here unless compute() contains an await
```

## `try/finally`

The `try/finally` construct ensures that a block of code in the `finally` clause executes regardless of whether an exception occurs in the `try` block. This is particularly useful for cleanup operations, such as logging or finalizing an action, ensuring that necessary steps are taken even if an error interrupts execution.

In the `placeOrder` function below:

- The `try` block processes an order and appends it to the `orders` array.
- The `finally` block logs that the order has been processed, ensuring that this message is always printed, whether the function succeeds or fails.

```motoko no-repl
public shared func placeOrder(order : Text) : async Text {
    try {
      Debug.print("Processing order: " # order);

      let newOrders = Array.tabulate<Text>(
        orders.size() + 1,
        func(i) {
          if (i < orders.size()) {orders[i]} else {order}
        }
      );

      orders := newOrders;
      return "Order received: " # order
    } finally {
      Debug.print("Order processed : " # order)
    };

};
```

## `try/catch/finally`

A `catch` block can be inserted inside a `try/finally` expression to catch an error.

```motoko no-repl
    try {
    // Code that might throw an error
    } catch (e) {
    // Handle the error
    } finally {
    // Cleanup code that always runs
    }
```

When `catch` is used, the `finally` clause is optional. The `catch` block only catches errors in certain scenarios:

1. Explicit throws: When code in the `try` block explicitly throws an error using the `throw` keyword:

```motoko no-repl
    try {
        throw Error.reject("Intentional error");
    } catch (e) {
        // This will catch the explicitly thrown error
    }
```

2. Errors from awaited calls: If an `await` expression in the `try` block returns an error:

```motoko no-repl
    try {
        let result = await someAsyncFunction(); // If this returns an error
    } catch (e) {
        // The error will be caught here
    }
```

`catch` blocks **do not** catch errors in the following scenarios:

1. Local traps
2. Pre-await traps in async functions
3. Traps after `await`

## Shared types

[Shared types](https://internetcomputer.org/docs/motoko/fundamentals/types/shared-types) allow actors to communicate with users and other [canisters](https://internetcomputer.org/docs/building-apps/essentials/canisters).

Motoko enforces type safety for shared communication, preventing unexpected modifications to data passed between actors.

```motoko no-repl
actor {
    // Since name is immutable, it can be safely shared across calls without risk of modification.
    public query func greet(name : Text) : async Text {
        return "Hello, " # name # "!";
    }
}
```

If you need to share a non-sharable type, you can serialize it into a sharable type. For example, if you want to share a `Map`, you can serialize it into an
array:

```motoko no-repl
import Map "mo:base/OrderedMap";
import Text "mo:base/Text";
import Iter "mo:base/Iter";

actor {
  let textMap = Map.Make<Text>(Text.compare);
  var map = textMap.empty<Nat>();
  
  // Initialize the non-sharable Map
  public func initialize() : async () {
    map := textMap.put(map, "key", 123);
  };
  
  // Return entries as an array of tuples (which is a shared type)
  public shared func getEntries() : async [(Text, Nat)] {
    Iter.toArray(textMap.entries(map))
  };
  
  // Get a specific value by key
  public shared func getValue(key : Text) : async ?Nat {
    textMap.get(map, key)
  };
}
```


<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />