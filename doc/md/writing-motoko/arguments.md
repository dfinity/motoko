---
sidebar_position: 4
---

# Arguments



Arguments can be passed to an actor's function for the function to use as input. Arguments can be [primitive values](../getting-started/basic-concepts#primitive-values), such as [`Int`](../base/Int.md), [`Nat`](../base/Nat.md), [`Bool`](../base/Bool.md), or [`Text`](../base/Text.md), or they can be non-primitive values such as tuples, arrays, or objects. To showcase a basic example of how an actor can accept an argument, this page will use an example Motoko actor that accepts multiple text arguments.

## Single text argument

First, define an argument that has a `location` function and the `name` argument with a `city` argument:

```motoko
persistent actor {
  public func location(city : Text) : async Text {
    return "Hello, " # city # "!";
  };
};
```

Once your canister is [deployed](https://internetcomputer.org/docs/current/developer-docs/getting-started/deploy-and-manage), you can call the `location` method in the program and pass your `city` argument of type [`Text`](../base/Text.md) by running the following command:

```
dfx canister call location_hello_backend location "San Francisco"
```

## Passing multiple arguments

You might want to try modifying the source code to return different results. For example, you might want to modify the `location` function to return multiple city names.

Revise the `location` function with two new functions:

```motoko
persistent actor {

  public func location(cities : [Text]) : async Text {
    return "Hello, from " # (debug_show cities) # "!";
  };

  public func location_pretty(cities : [Text]) : async Text {
    var str = "Hello from ";
    for (city in cities.vals()) {
        str := str # city # ", ";
    };
    return str # "bon voyage!";
  }
};

```

You might notice that [`Text`](../base/Text.md) in this code example is enclosed by square (`[ ]`) brackets. By itself, [`Text`](../base/Text.md) represents a (UTF-8 encoded) sequence of Unicode characters. Placing square brackets around a type describes an **array** of that type. In this context, therefore, `[Text]` indicates an array of text values, enabling the program to accept multiple text values as an array.

For information about the functions that perform operations on arrays, see the description of the [Array module](../base/Array.md) in the Motoko base library or the **Motoko programming language reference**. For another example focused on the use of arrays, see the [quick sort](https://github.com/dfinity/examples/tree/master/motoko/quicksort) project in the [examples](https://github.com/dfinity/examples/) repository.

Call the `location` method in the program and pass your `city` argument using the Candid interface description syntax by running the following command:

```
dfx canister call favorite_cities location '(vec {"San Francisco";"Paris";"Rome"})'
```

The command uses the Candid interface description syntax `(vec { val1; val2; val3; })` to return a vector of values. For more information about the Candid interface description language, see the [Candid](https://internetcomputer.org/docs/current/developer-docs/smart-contracts/candid/candid-concepts) language guide.

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />
