---
sidebar_position: 3
---

# Object declarations

In Motoko, an object is a collection of named fields that can hold values or functions.

## Defining an object

Objects are declared using the `object` keyword, followed by a block containing field declarations. Fields can be immutable or mutable, and can store data or functions.

```motoko
let alice = object {
// name is an immutable public field of type Text.
    public let name = "Alice";
// age is a mutable public field of type Int.
    public var age = 30;
// greet is a public function that returns a greeting message.
    public func greet() : Text {
        "Hello, My name is " # name # "!"
    };
};
```


## Accessing object fields

Fields and methods of an object are accessed using dot notation:

```motoko
let alice = object {
    public let name = "Alice";
    public var age = 30;

    public func greet() : Text {
        "Hello, my name is " # name # "!"
    };
    public func birthday(): Text{
      age += 1;
      "I had a birthday I'm now " # debug_show(age) # "years old!"
    }
};
Debug.print(alice.greet());
Debug.print(alice.birthday());
```

## Comparing records vs. objects

| Feature | Record | Object |
|---------|--------|--------|
| Stores data | Yes | Yes |
| Stores functions | No | Yes |
| Mutability | Supports `let` (immutable) and `var` (mutable) | Supports `let` (immutable) and `var` (mutable) |
| Supports field visibility (`public`, `private`) | No | Yes |
| Combination (`and`, `with`) | Yes | Yes |
| Supports async behavior | No | Yes (supports `async` functions) |

### Record example

```motoko
let person = {
    var name : Text = "Alice";
    var age : Nat = 25;
};

Debug.print(person.name); // "Alice"
person.age += 1; // Updates age
```

Records store only data; they do not contain functions. Fields are always accessible using dot notation (`person.name`).
Records do not support visibility modifiers (`public`, `private`). They support `and` and `with` for combination and modification.

### Object example

```motoko
let alice = object {
    public let name = "Alice";
    public var age = 30;

    public func greet() : async Text {
        "Hello, my name is " # name # "!"
    };
};

Debug.print(alice.greet()); // "Hello, my name is Alice!"
```

Objects extend records by supporting functions. Fields can have explicit visibility modifiers, such as `public` or `private`.

Objects support async functions and structured behavior.
