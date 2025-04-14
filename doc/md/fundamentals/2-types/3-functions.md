---
sidebar_position: 3
---

# Function types

Motoko provides different types of functions based on where in the program they execute and how they interact with the system. Understanding these distinctions is essential when designing efficient and scalable canister logic.

## Function keywords

| Keyword  | Function  |
|-------------|--------------|
| `public`    | Makes a function callable by users or other [canisters](https://internetcomputer.org/docs/building-apps/essentials/canisters). |
| `shared`    | Enables [inter-canister](https://internetcomputer.org/docs/motoko/fundamentals/messaging) calls and provides access to the caller’s identity. |
| `async`     | Runs the function [asynchronously](https://internetcomputer.org/docs/motoko/fundamentals/actors-async#async--await) and returns a future result. |
| `query`     | Optimized for reading data but cannot modify [state](https://internetcomputer.org/docs/motoko/fundamentals/state). |

## Function comparison

| Function type                | Mutates [state](https://internetcomputer.org/docs/motoko/fundamentals/state) | Calls updates | Calls queries | Asynchronous | External calls |
|------------------------------|---------------|------------------|------------------|---------------|---------------|
| Local                    | No            | Yes              | Yes              | No            | No            |
| Generic                  | No            | Yes              | Yes              | No            | No            |
| Shared update            | Yes           | Yes              | Yes              | Yes           | Yes           |
| Shared query             | No            | No               | Yes              | Yes           | Yes           |
| Shared composite query   | No            | No               | Yes              | Yes           | Yes           |

## Local functions

Local functions run within the canister's [actor](https://internetcomputer.org/docs/motoko/fundamentals/actors-async). They do not make calls to other [canisters](https://internetcomputer.org/docs/building-apps/essentials/canisters). Local functions are processed fast and synchronously.

```motoko no-repl
func add(a: Nat, b: Nat): Nat {
    a + b
};

let sum: Nat = add(5, 3); // Synchronous execution
```

**Example use case:** Simple computations that do not require network calls.

## Generic functions

Generic functions allow the use of type parameters, making them more flexible for using different data types.

```motoko no-repl
func identity<T>(x: T): T {
    x
};

let num: Nat = identity<Nat>(42);
let text: Text = identity<Text>("Hello");
```

## Shared functions

Shared functions can be called from outside the canister (e.g., by users or other [canisters](https://internetcomputer.org/docs/building-apps/essentials/canisters)). These are processed [asynchronously](https://internetcomputer.org/docs/motoko/fundamentals/actors-async#async--await) because they interact with the network.

```motoko no-repl
  public shared func getBalance(): async Nat {
    return 100;
  };
```

**Example use case:** Reusable logic for different types.

## Query functions

[Query](https://internetcomputer.org/docs/building-apps/interact-with-canisters/query-calls) functions are designed for retrieving data. They cannot modify [state](https://internetcomputer.org/docs/motoko/fundamentals/state) and execute faster than [update](https://internetcomputer.org/docs/building-apps/interact-with-canisters/update-calls) functions because they do not go through consensus. Query functions are identified with the `query` keyword. Any function without the `query` keyword is an [update](https://internetcomputer.org/docs/building-apps/interact-with-canisters/update-calls) function.

```motoko no-repl
  public query func greet(name : Text) : async Text {
    return greeting # name # "!";
  };
```

### Shared query functions

```motoko
actor Bank {
    stable var balance: Nat = 100;

    public query func getBalance(): async Nat {
        return balance;
    };
};
await Bank.getBalance();
```

**Example use case:** Fetching data quickly without modifying the canister [state](https://internetcomputer.org/docs/motoko/fundamentals/state).


### Composite queries

[Composite queries](https://internetcomputer.org/docs/building-apps/interact-with-canisters/query-calls#composite-queries) chain multiple query calls together within the same function.

```motoko no-repl
import Nat "mo:base/Nat";
actor Counter {
  stable var count : Nat = 0;
  public query func getCount() : async Nat {
    return count
  };
  public composite query func isEven() : async Bool {
    return (await getCount()) % 2 == 0
  };
  public composite query func summary() : async Text {
    let current = await getCount();
    let even = await isEven();
    if (even) {
      return "Count is " # Nat.toText(current) # " and it is even."
    } else {
      return "Count is " # Nat.toText(current) # " and it is odd."
    }
  };
}
```

### Shared composite query functions

```motoko no-repl
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

**Example use case:** Efficiently combining multiple queries while avoiding [inter-canister](https://internetcomputer.org/docs/motoko/fundamentals/messaging) update calls.


## Update functions

[Update](https://internetcomputer.org/docs/building-apps/interact-with-canisters/update-calls) functions modify a canister’s [state](https://internetcomputer.org/docs/motoko/fundamentals/state) and must go through consensus before the result is returned. Any function without the identifier `query` is an update function by default.

```motoko no-repl
  public func setGreeting(prefix : Text) : async () {
    greeting := prefix;
  };
```

**Example use case:** Exposing public endpoints for [inter-canister](https://internetcomputer.org/docs/motoko/fundamentals/messaging) or [frontend](/docs/building-apps/frontends/using-an-asset-canister) interactions.

### Shared update functions

```motoko
actor Bank {
    stable var balance: Nat = 0;

    public shared func deposit(amount: Nat): async () {
        balance += amount;
    };
};
```

**Example use case**: Transactions, user [state](https://internetcomputer.org/docs/motoko/fundamentals/state) updates, or anything that modifies persistent data.

## Passing arguments to functions

An argument is a value passed to a function when called. Arguments enable functions to process incoming data during execution.

### Single argument

A function can take a single argument of a specific type.

```motoko no-repl
  public func increment(amount: Nat) : async Nat {
    count += amount;
    return count;
  }
```

### Multiple arguments

Functions can accept multiple arguments by separating them with commas.

```motoko no-repl
  public func add(x: Nat, y: Nat) : async Nat {
    return x + y;
  }
```

### Using a record as an argument

Multiple values can be passed as a single argument by encapsulating them within a [record](https://internetcomputer.org/docs/motoko/fundamentals/types/records) type.

```motoko no-repl
type UserData = { name: Text; age: Nat };
  public func createUser(userData: UserData) : async Text {
    return userData.name;
  }
```

### Using an array as an argument

A collection of values can be passed as a single array argument.

```motoko no-repl
  public func sum(numbers: [Nat]) : async Nat {
    var total : Nat = 0;
    for (num in numbers.vals()) { total += num };
    return total;
  }
```

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />