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

actor PizzaParlor {
    stable var orders: [Text] = [];

    public shared func placeOrder(order: Text): async Text {
        // Use Array.tabulate to create a new array with the additional element
        let newOrders = Array.tabulate<Text>(orders.size() + 1, func(i) {
            if (i < orders.size()) { orders[i] } else { order }
        });
        orders := newOrders;
        return "Order received: " # order;
    };

    public shared func makePizza(): async Text {
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

    public query func getOrders(): async [Text] {
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

Customers can place multiple orders at the same time. However, when they ask for an update on their delivery status, responses must be generated sequentially based on the preparation time. Instead of constantly tracking delivery updates, `async*` can be used to calculate the estimated delivery time only when requested.

```motoko no-repl
// async* function to calculate delivery time on demand
func checkDelivery(order: Text): async* Text {
        let estimatedTime = 15 + (orders.size() * 5); // Base time + 5 mins per pending order
        return "Your " # order # " will arrive in " # Nat.toText(estimatedTime) # " minutes.";
    };

    // Multiple users can check their delivery status asynchronously
    public shared func getDeliveryStatus(order: Text): async Text {
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

## `try/finally`

The `try/finally` construct ensures that a block of code in the `finally` clause executes regardless of whether an exception occurs in the `try` block. This is particularly useful for cleanup operations, such as logging or finalizing an action, ensuring that necessary steps are taken even if an error interrupts execution.

In the `placeOrder` function below:

- The `try` block processes an order and appends it to the `orders` array.
- The `finally` block logs that the order has been processed, ensuring that this message is always printed, whether the function succeeds or fails.

``` motoko no-repl
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

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />