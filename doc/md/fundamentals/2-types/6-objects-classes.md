---
sidebar_position: 6
---

# Objects and classes

## Objects

An object, similar to a [record](https://internetcomputer.org/docs/motoko/fundamentals/types/records), stores structured data with optional mutable fields and supports methods, including [asynchronous](https://internetcomputer.org/docs/motoko/fundamentals/actors-async#async--await) behavior. Unlike records, objects can encapsulate or share their [state](https://internetcomputer.org/docs/motoko/fundamentals/state) and behavior using `public` and `private` visibility modifiers. However, they cannot be instantiated independently, such as `object()`.

```motoko no-repl
object Account {
    var balance: Nat = 1000;

    func deposit(amount: Nat): Nat {
        balance += amount;
        balance
    };

    func withdraw(amount: Nat): ?Nat {
        if (amount > balance) { return null };
        balance -= amount;
        ?balance
    };
}
```

## Classes

A class acts as a blueprint for creating multiple objects with independent [state](https://internetcomputer.org/docs/motoko/fundamentals/state).

```motoko no-repl
class Account(initialBalance: Nat) {
    var balance: Nat = initialBalance;

    public func deposit(amount: Nat): async Nat {
        balance += amount;
        balance
    };

    public func withdraw(amount: Nat): ?Nat {
        if (amount > balance) { return null };
        balance -= amount;
        ?balance
    };
};

// Creating multiple accounts
let account1 = Account(500);
let account2 = Account(1000);
```

### Object classes

An object class defines a blueprint for multiple objects.

```motoko no-repl
object class Account(initialBalance: Nat) {
    var balance: Nat = initialBalance;

    public func deposit(amount: Nat): Nat {
        balance += amount;
        balance
    };

    public func withdraw(amount: Nat): ?Nat {
        if (amount > balance) { return null };
        balance -= amount;
        ?balance
    };
}
```

## Modules

A module provides reusable utility functions and encapsulates both [state](https://internetcomputer.org/docs/motoko/fundamentals/state) and behavior, module [state](https://internetcomputer.org/docs/motoko/fundamentals/state) is not persistent unless explicitly stored. It supports [asynchronous](https://internetcomputer.org/docs/motoko/fundamentals/actors-async#async--await) operations, making it suitable for managing complex functionality across different parts of a program.

```motoko no-repl
module CurrencyConverter {
    public func toUSD(amount: Nat): Float {
        return Float.fromInt(amount) * 1.1;
    };
}
```


### Module classes

A module class can be used to produce multiple modules with different configurations.

```motoko no-repl
module class ExchangeRate(baseRate: Float) {
    public func convert(amount: Nat): Float {
        return Float.fromInt(amount) * baseRate;
    };
}

// Creating different currency converters
let usdConverter = ExchangeRate(1.1);
let eurConverter = ExchangeRate(0.9);

Debug.print(Float.toText(usdConverter.convert(100)));  // "110.0"
Debug.print(Float.toText(eurConverter.convert(100)));  // "90.0"
```


## Object subtyping

Object subtyping allows objects with more fields to be treated as subtypes of objects with fewer fields. This enables flexibility in function arguments and object compatibility.

Objects with fewer fields are more general, while objects with additional fields are subtypes of more general types.

| Generality         | Object type | Relation |
|--------------------|------------------------------------------|----------------------|
| Most general   | `object basicAccount = { getBalance : () -> Nat }` | Subtype of `standardAccount`  |
| Middle generality | `object standardAccount { getBalance : () -> Nat; deposit : Nat -> () }` | Subtype of `premiumAccount` |
| Least general  | `{ object premiumAccount getBalance : () -> Nat; deposit : Nat -> (); withdraw : Nat -> Bool }` | Supertype |

A function expecting `{ getBalance : () -> Nat }` can accept any of the above, since all contain at least that method. However, a function requiring `{ withdraw : Nat -> Bool }` cannot accept more general types that lack this method.

- A subtype must be usable wherever its supertype is expected.
- The more general object has fewer methods because it makes fewer assumptions about available functionality.

`basicAccount <: standardAccount <: premiumAccount`, or equivalently:  `basicAccount` is a subtype of `standardAccount`, which is a subtype of `premiumAccount`.

A function expecting `premiumAccount` expects `withdraw`, so it cannot accept `basicAccount`.
However, a function expecting `basicAccount` only needs `getBalance`, so it can accept all three objects.

```motoko no-repl
func printBalance(account: { getBalance : () -> Nat }) {
  Debug.print("Balance: " # Nat.toText(account.getBalance()));
};

// Works, because all have getBalance
printBalance(basicAccount);
printBalance(standardAccount);
printBalance(premiumAccount);
```

```motoko no-repl
func withdrawFromAccount(account: { withdraw : Nat -> Bool }) {
  let success = account.withdraw(100);
  Debug.print(if success then "Withdrawal successful" else "Insufficient funds");
};

// Works only for premiumAccount, fails for others
withdrawFromAccount(premiumAccount);  // Works
withdrawFromAccount(standardAccount); // Fails (missing withdraw)
withdrawFromAccount(basicAccount);    // Fails (missing withdraw)
```

[Learn more about subtyping](https://internetcomputer.org/docs/motoko/fundamentals/types/subtyping).

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />