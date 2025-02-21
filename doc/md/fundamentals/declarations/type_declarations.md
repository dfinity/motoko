---
sidebar_position: 5
---

# Type declarations 

A type declaration in Motoko allows defining custom types that improve readability, reusability, and structure in code. These can represent records, variants, objects, or parameterized (generic) types. Motoko enforces productivity and non-expansiveness in type declarations to ensure well-formed and valid types.

## Defining a type

The `type` keyword assigns a name to a type, making it reusable.

```motoko
type Age = Nat;
type Username = Text;
```

- `Age` is an alias for `Nat`.  
- `Username` is an alias for `Text`.  

These types can then be used in function definitions:

```motoko
func greet(name: Username, age: Age) : Text {
    "Hello, " # name # "! You are " # Nat.toText(age) # " years old."
}
```

## Record types

A type can represent a structured record with labeled fields.

```motoko
type Person = {
    firstName: Text;
    lastName: Text;
    age: Nat;
};

let alice: Person = {
    firstName = "Alice";
    lastName = "Smith";
    age = 30;
};
```

- `Person` is a reusable record type.
- `alice` is an instance of `Person`.

## Variant types

A type can also define variants, representing different possible states.

```motoko
// allows only one of its variants at a time.
type Status = {
    #Active;
    #Inactive;
// carries an additional `Text` value.
    #Banned : Text;
};

let userStatus: Status = #Active;
let bannedUser: Status = #Banned("Violation of rules");
```


## Parameterized (generic) types

Type declarations can be parameterized to work with multiple types.

```motoko
// `Box<T> is a generic type where T represents any type.
type Box<T> = {
    value: T;
};
// numberBox stores a Nat and textBox stores a Text.
let numberBox: Box<Nat> = { value = 42 };
let textBox: Box<Text> = { value = "Hello" };
```


This ensures flexibility while keeping type safety.

## Recursive types and productivity

Motoko allows recursive type definitions, provided they are productive.

### Productive recursive type

```motoko
type List<T> = ?(T, List<T>);
```

`List<T>` defines a linked list structure. Each list node contains a value (`T`) and another `List<T>` or `null` (end of the list). This type is non-expansive and accepted.

### Non-productive recursive type

```motoko
type C = C; // This definition infinitely refers to itself
```

This type will never resolve to a concrete type. It is non-productive and rejected.

## Expansiveness in type definitions

Motoko enforces non-expansiveness to prevent type definitions from expanding indefinitely.

### Non-expansive

```motoko
type List<T> = ?(T, List<T>);
```

Expands without introducing a larger type.

### Expansive

```motoko
type Seq<T> = ?(T, Seq<[T]>);
```

Expands by wrapping `T` inside `[T]`, growing the type. This is expansive and not allowed.

## References

-[Record](../expressions/control-flow/record.md)
-[Variant](../expressions/control-flow/variant.md)
