---
sidebar_position: 3
---

# Arguments

## Overview

Arguments can be passed to an actor's function for the function to use as input. Arguments can be [primitive values](/docs/current/motoko/main/getting-started/basic-concepts#primitive-values), such as `Int`, `Nat`, `Bool`, or `Text`, or they can be non-primitive values such as tuples, arrays, or objects. To showcase a basic example of how an actor can accept an argument, this page will use an example Motoko actor that accepts multiple text arguments.

## Single text argument

First, define an argument that has a `location` function and the `name` argument with a `city` argument:

```motoko
actor {
public func location(city : Text) : async Text {
    return "Hello, " # city # "!";
};
};
```

Once your canister is [deployed](/docs/current/developer-docs/getting-started/deploy/local), you can call the `location` method in the program and pass your `city` argument of type `text` by running the following command:

```
dfx canister call location_hello_backend location "San Francisco"
```

Because the argument in this case includes a space between `San` and `Francisco`, you need to enclose the argument in quotes. The command displays output similar to the following:

```
("Hello, San Francisco!")
```

If the argument did not contain a space that required enclosing the text inside of quotation marks, you could allow the Candid interface description language to infer the data type like this:

```
dfx canister call location_hello_backend location Paris
```

Candid infers the data type as `Text` and returns the output from your program as text like this:

```
("Hello, Paris!")
```

Call the `location` method in the program and pass your `city` argument explicitly using the Candid interface description language syntax for Text arguments:

```
dfx canister call location_hello_backend location '("San Francisco and Paris")'
```

The command displays output similar to the following:

```
("Hello, San Francisco and Paris!")
```

Because your program only accepts a single text argument, specifying multiple strings returns only the first argument. For example, if you try this command:

```
dfx canister call location_hello_backend location '("San Francisco","Paris","Rome")'
```

Only the first argument—`("Hello, San Francisco!")`—is returned.

## Passing multiple arguments

You might want to try modifying the source code to return different results. For example, you might want to modify the `location` function to return multiple city names.

Revise the `location` function with two new functions:

```motoko
actor {

public func location(cities : [Text]) : async Text {
    return "Hello, from " # (debug_show cities) # "!";
};

public func location_pretty(cities : [Text]) : async Text {
    var str = "Hello from ";
    for (city in cities.vals()) {
    str := str # city #", ";
    };
    return str # "bon voyage!";
}
};

```

You might notice that `Text` in this code example is enclosed by square (`[ ]`) brackets. By itself, `Text` represents a collection of UTF-8 characters. The square brackets around a type indicate that it is an **array** of that type. In this context, therefore, `[Text]` indicates an array of a collection of UTF-8 characters, enabling the program to accept and return multiple text strings.

The code sample also uses the basic format of an `apply` operation for the array, which can be abstracted as:

```motoko
public func apply<A, B>(fs : [A -> B], xs : [A]) : [B] {
    var ys : [B] = [];
    for (f in fs.vals()) {
        ys := append<B>(ys, map<A, B>(f, xs));
    };
    ys;
};
```

For information about the functions that perform operations on arrays, see the description of the Array module in the Motoko base library or the **Motoko programming language reference**. For another example focused on the use of arrays, see the [quick sort](https://github.com/dfinity/examples/tree/master/motoko/quicksort) project in the [examples](https://github.com/dfinity/examples/) repository.

Call the `location` method in the program and pass your `city` argument using the Candid interface description syntax by running the following command:

```
dfx canister call favorite_cities location '(vec {"San Francisco";"Paris";"Rome"})'
```

The command uses the Candid interface description syntax `(vec { val1; val2; val3; })` to return a vector of values. For more information about the Candid interface description language, see the [Candid](/docs/current/developer-docs/smart-contracts/candid/index) language guide.

This command displays output similar to the following:

```
("Hello, from ["San Francisco", "Paris", "Rome"]!")
```

Call the `location_pretty` method in the program and pass your `city` argument using the interface description syntax by running the following command:

```
dfx canister call favorite_cities location_pretty '(vec {"San Francisco";"Paris";"Rome"})'
```

The command displays output similar to the following:

```
("Hello from San Francisco, Paris, Rome, bon voyage!")
```