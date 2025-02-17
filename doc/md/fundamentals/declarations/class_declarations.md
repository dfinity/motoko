# Classes

A class in Motoko is a blueprint for creating objects. It encapsulates state (fields) and behavior (methods) while allowing instances to be created with specific data. Unlike records and objects, classes support constructors, enabling each instance to have unique values upon creation.

## Defining a class

A class is declared using the `class` keyword. It defines:  

- Fields to store data
- Methods to perform operations
- A constructor to initialize instance values

```motoko
class Person(name: Text, age: Nat) {
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
class Person(name: Text, age: Nat) {
    public func greet() : Text {
        "Hello, my name is " # name # " and I am " # Nat.toText(age) # " years old."
    };
};

let alice = Person("Alice", 30);
Debug.print(alice.greet());
```

- `Person("Alice", 30)` creates a new instance of `Person`.  
- `greet()` is called on the instance `alice`.  
