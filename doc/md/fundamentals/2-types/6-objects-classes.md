---
sidebar_position: 6
---

# Objects & classes

## Objects

In Motoko, an object is just a collection of named fields, holding values. These values can either be plain data, or function values. In addition, each field can be mutable or immutable.

A simple object containing just fields of data is like a record in a database.  
Motoko's light-weight record syntax make it easy do construct such objects [records](https://internetcomputer.org/docs/motoko/fundamentals/types/records).  

When fields contain function values, Motoko objects can represent traditional objects with methods, familiar from object-oriented programming (OOP).  
From an OOP perspective, an object is an abstraction, defined by the behavior of its methods. Methods are typically used to modify or observe some encapsulated (i.e. hidden) state of an object.  

In addition to the record syntax, Motoko let's you defined objects from a block of declarations.  
The declarations in the block can be `public` or `private`, with `private` the default.  
The public declarations become fields of the object, all private declarations are hidden.  

An object, similar to a [record](https://internetcomputer.org/docs/motoko/fundamentals/types/records), stores structured data with optional mutable fields and supports methods, including [asynchronous](https://internetcomputer.org/docs/motoko/fundamentals/actors-async#async--await) behavior. Unlike records, objects can encapsulate or share their [state](https://internetcomputer.org/docs/motoko/fundamentals/state) and behavior using `public` and `private` visibility modifiers. However, they cannot be instantiated independently, such as `object()` use `object class()` instead.

```motoko no-repl
object Account {
    var balance : Nat = 1000;

   public  func deposit(amount : Nat) : Nat {  
        balance += amount;
        balance
    };

    func withdraw(amount : Nat) : ?Nat {
        if (amount > balance) { return null };
        balance -= amount;
        ?balance
    };
}
```

## Classes

A class acts as a blueprint for creating multiple objects with independent [state](https://internetcomputer.org/docs/motoko/fundamentals/state).

```motoko no-repl
class Account(initialBalance : Nat) {
    var balance = initialBalance;

    public func deposit(amount : Nat) : async Nat {
        balance += amount;
        balance
    };

    public func withdraw(amount : Nat) : ?Nat {
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

An object class defines a blueprint for multiple objects. The above is just short-hand for an `object` class. Motoko also support module and actor classes.  

```motoko no-repl
object class Account(initialBalance : Nat) {
    var balance = initialBalance;

    public func deposit(amount : Nat) : Nat {
        balance += amount;
        balance
    };

    public func withdraw(amount : Nat) : ?Nat {
        if (amount > balance) { return null };
        balance -= amount;
        ?balance
    };
}
```

## Modules

Modules are similar to objects, containing public and private declarations, but are restricted to be stateless. They are typically used to implement libraries of types, functions and values, and, unlike objects, can be imported from other files.  

```motoko no-repl
module CurrencyConverter {
    public func toUSD(amount : Nat) : Float {
        return Float.fromInt(amount) * 1.1;
    };
}
```

### Module classes

A module class can be used to produce multiple modules with different configurations.

```motoko no-repl
module class ExchangeRate(baseRate : Float) {
    public func convert(amount : Nat) : Float {
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

| Specificity Level | Description | Relationship |
|-------------------|-------------|--------------|
| Most general | `type basicAccount = { getBalance : () -> Nat }` | Subtype of `{}` |
| Middle generality | `type standardAccount = { getBalance : () -> Nat; deposit : Nat -> () }` | Subtype of `basicAccount` |
| Least general | `type premiumAccount = { getBalance : () -> Nat; deposit : Nat -> (); withdraw : Nat -> Bool }` | Subtype of `standardAccount` |

`basicAccount` is the most general type of account, because `standardAccount` and `premiumAccount` can both be used as `basicAccount`s.  

A function expecting `{ getBalance : () -> Nat }` can accept any of the above, since all contain at least that method. However, a function requiring `{ withdraw : Nat -> Bool }` cannot accept more general types that lack this method.

- A subtype must be usable wherever its supertype is expected.
- The more general object has fewer methods because it makes fewer assumptions about available functionality.

`premiumAccount <: standardAccount <: basicAccount`, or equivalently:  `premiumAccount` is a subtype of `standardAccount`, which is a subtype of `basicAccount`.  

A function expecting `premiumAccount` expects `withdraw`, so it cannot accept `basicAccount`.
However, a function expecting `basicAccount` only needs `getBalance`, so it can accept all three type of objects.  

```motoko no-repl
func printBalance(account : { getBalance : () -> Nat }) {
  Debug.print("Balance: " # Nat.toText(account.getBalance()));
};

// Works, because all have getBalance
printBalance(basicAccount);
printBalance(standardAccount);
printBalance(premiumAccount);
```

```motoko no-repl
func withdrawFromAccount(account : { withdraw : Nat -> Bool }) {
  let success = account.withdraw(100);
  Debug.print(if success then "Withdrawal successful" else "Insufficient funds");
};

// Works only for premiumAccount, fails for others
withdrawFromAccount(premiumAccount);  // Works
withdrawFromAccount(standardAccount); // type error: (missing withdraw)  
withdrawFromAccount(basicAccount);    // type error: (missing withdraw)  
```

[Learn more about subtyping](https://internetcomputer.org/docs/motoko/fundamentals/types/subtyping).

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />