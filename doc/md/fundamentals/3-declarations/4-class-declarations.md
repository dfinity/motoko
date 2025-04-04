---
sidebar_position: 4
---

# Class declarations

A class in Motoko is a blueprint for creating [objects](./3-object_declaration.md). It encapsulates state (fields) and behavior (methods) while allowing instances to be created with specific data. Unlike records and objects, classes support constructors, enabling each instance to have unique values upon creation.

## Defining a class

A class is declared using the `class` keyword. It defines:  

- Fields to store data
- Methods to perform operations
- A constructor to initialize instance values

```motoko
class Ghost(name: Text, age: Nat) {
    public func greet() : Text {
        "Hello, my name is " # name # " and I am " # Nat.toText(age) # " years old."
    };
};
```

- `name` and `age` are parameters that initialize the instance.
- `greet` is a method that returns a message.

## Creating instances

To create an instance of a class, call the **constructor** using the class name:

```motoko
class Ghost(name: Text, age: Nat) {
    public func greet() : Text {
        "Hello, my name is " # name # " and I am " # Nat.toText(age) # " years old."
    };
};

let motoko = Ghost("Motoko", 30);
Debug.print(motoko.greet());
```

- `Ghost("Motoko", 30)` creates a new instance of `Ghost`.
- `greet()` is called on the instance `motoko`.

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />