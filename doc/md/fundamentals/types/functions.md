---
sidebar_position: 8
---

# Functions  

Motoko provides different types of functions based on where they execute and how they interact with the system. Understanding these distinctions is essential when designing efficient and scalable canister logic.

## Function comparison  

| Function type                | Mutates state | Calls updates | Calls queries | Asynchronous | External calls |
|------------------------------|---------------|------------------|------------------|---------------|---------------|
| Local                    | No            | Yes              | Yes              | No            | No            |
| Generic                  | No            | Yes              | Yes              | No            | No            |
| Shared update            | Yes           | Yes              | Yes              | Yes           | Yes           |
| Shared query             | No            | No               | Yes              | Yes           | Yes           |
| Shared composite query   | No            | No               | Yes              | Yes           | Yes           |

## Local functions  

A local function runs within the same actor and does not require inter-canister messaging. It is fast and synchronous.

```motoko norepl
func add(a: Nat, b: Nat): Nat {
    a + b
};

let sum: Nat = add(5, 3); // Synchronous execution
```

Use case: Simple computations that do not require network calls.  

## Generic functions

A generic function allows type parameters, making it more flexible for different data types.

```motoko
func identity<T>(x: T): T {
    x
};

let num: Nat = identity<Nat>(42);
let text: Text = identity<Text>("Hello");
```

Use case: Reusable logic for different types.  

## Shared functions  

A shared function can be called from outside the canister (e.g., by users or other canisters). These are asynchronous because they involve network interactions.  

```motoko
public shared func getBalance(): async Nat {
    return 100;
};
```

Use case: Exposing public endpoints for inter-canister or frontend interactions.

## Shared update functions

A shared update function modifies the canister’s state and must persist data changes.  

```motoko
actor Bank {
    stable var balance: Nat = 0;

    public shared func deposit(amount: Nat): async () {
        balance += amount;
    };
};
```

Use case: Transactions, user state updates, or anything that modifies persistent data.  

## Shared query functions

A shared query function allows fast, read-only access to the canister’s state. It is asynchronous but executes much faster than an update function because it does not modify the state and does not undergo concensus.

```motoko
actor Bank {
    stable var balance: Nat = 100;

    public query func getBalance(): async Nat {
        return balance;
    };
};
await Bank.
```

Use case: Fetching data quickly without modifying the canister state.  

## Shared composite query functions  

A shared composite query is a query function that can call other query functions, allowing query chaining without converting into an update function.

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

Use case: Efficiently combining multiple queries while avoiding inter-canister update calls.
