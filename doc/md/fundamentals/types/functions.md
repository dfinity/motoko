---
sidebar_position: 8
---

# Functions  

Motoko provides different types of functions based on where in the program they execute and how they interact with the system. Understanding these distinctions is essential when designing efficient and scalable canister logic.

## Function comparison  

| Function type                | Mutates state | Calls updates | Calls queries | Asynchronous | External calls |
|------------------------------|---------------|------------------|------------------|---------------|---------------|
| Local                    | No            | Yes              | Yes              | No            | No            |
| Generic                  | No            | Yes              | Yes              | No            | No            |
| Shared update            | Yes           | Yes              | Yes              | Yes           | Yes           |
| Shared query             | No            | No               | Yes              | Yes           | Yes           |
| Shared composite query   | No            | No               | Yes              | Yes           | Yes           |

## Local functions  

Local functions run within the canister's actor. They does not make calls to other canisters. Local functions are processed fast and synchronously.

```motoko norepl
func add(a: Nat, b: Nat): Nat {
    a + b
};

let sum: Nat = add(5, 3); // Synchronous execution
```

Example use case: Simple computations that do not require network calls.  

## Generic functions

Generic functions allow the use of type parameters, making them more flexible for using different data types.

```motoko
func identity<T>(x: T): T {
    x
};

let num: Nat = identity<Nat>(42);
let text: Text = identity<Text>("Hello");
```

Example use case: Reusable logic for different types.  

## Shared functions  

Shared functions can be called from outside the canister (e.g., by users or other canisters). These are processed asynchronously because they require interacting with the network.

```motoko
public shared func getBalance(): async Nat {
    return 100;
};
```

Example use case: Exposing public endpoints for inter-canister or frontend interactions.

## Shared update functions

Shared update functions modify a canister’s state and must persist data changes.  

```motoko
actor Bank {
    stable var balance: Nat = 0;

    public shared func deposit(amount: Nat): async () {
        balance += amount;
    };
};
```

Example use cases: Transactions, user state updates, or anything that modifies persistent data.  

## Shared query functions

Shared query functions allow fast, read-only access to the canister’s state. They are asynchronous but execute much faster than an update function because they do not modify the state and do not go through consensus.

```motoko
actor Bank {
    stable var balance: Nat = 100;

    public query func getBalance(): async Nat {
        return balance;
    };
};
await Bank.getBalance();
```

Example use case: Fetching data quickly without modifying the canister state.  

## Shared composite query functions  

Shared composite queries are a query function that can call other query functions, allowing query chains without being converted into an update function.

```motoko
actor Bank {
    stable var balance: Nat = 100;

    public query func getBalance(): async Nat {
        return balance;
    };

    public composite query func doubleBalance(): async Nat {
        let b: Nat = await getBalance();
        return b * 2;
    };
};
await Bank.doubleBalance();
```

Example use case: Efficiently combining multiple queries while avoiding inter-canister update calls.
