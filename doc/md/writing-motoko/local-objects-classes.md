---
sidebar_position: 13
---

# Objects and classes

<!--
TODO: Move examples into doc/modules/language-guide/examples
-->


In Motoko, an object is just a collection of named fields, holding values. These values can either be plain data, or function values. In addition, each field can be mutable or immutable.

A simple object containing just fields of data is like a record in a database.
When the fields are immutable and have shared types, the object itself is shareable and can be sent and received from shared functions.

When fields contain function values, Motoko objects can represent traditional objects with methods, familiar from object-oriented programming (OOP).
From an OOP perspective, an object is an abstraction, defined by the behavior of its methods. Methods are typically used to modify or observe some encapsulated (i.e. hidden) state of an object.
Motoko programs benefit from the ability to encapsulate state as objects with abstract types. The [mutable state](mutable-state.md) introduces declarations of mutable state in the form of `var`-declared variables. Using such declarations privately in its body, an object can encapsulate the state, declaring public methods that access and update it.

By design, objects with mutable fields or methods cannot be sent to remote actors. If that were allowed, a receiver would either have to receive a remote reference to the local object, breaking the isolation of the actor model by allowing remote updates to local state. Or, the receiver would have to receive a copy of the local object. Then, the effect of any changes to the copy would not be reflected in the original, leading to confusion.


To compensate for this necessary limitation, `actor` objects are shareable, but always execute remotely. They communicate with shareable Motoko data only. Local objects interact in less restricted ways with themselves, and can pass any Motoko data to each other’s methods, including other objects. In most other ways, local objects and classes are non-shareable counterparts to actor objects and classes.

## Example

The following example illustrates a general evolution path for Motoko programs. Each object has the potential to be refactored into a service by refactoring the local object into an actor object.

Consider the following object declaration of the object value `counter`:

``` motoko
object counter {
  var count = 0;
  public func inc() { count += 1 };
  public func read() : Nat { count };
  public func bump() : Nat {
    inc();
    read()
  };
};
```

This declaration introduces a single object instance named `counter`. The developer exposes three public functions `inc`, `read` and `bump` using keyword `public` to declare each in the object body. The body of the object, like a block expression, consists of a list of declarations.

In addition to these three functions, the object has one private mutable variable `count`, which holds the current count and is initially zero.

## Object types

This object `counter` has the following object type, written as a list of field-type pairs, enclosed in braces `{` and `}`:

``` motoko no-repl
{
  inc  : () -> () ;
  read : () -> Nat ;
  bump : () -> Nat ;
}
```

Each field type consists of an identifier, a colon `:`, and a type for the field content. Here, each field is a function, and thus has an arrow type form (`_ -> _`).

In the declaration of `object`, the variable `count` was explicitly declared neither as `public` nor as `private`.

By default, all declarations in an object block are `private`. Consequently, the type for `count` does not appear in the type of the object. Its name and presence are both inaccessible from the outside.

By not exposing this implementation detail, the object has a more general type with fewer fields, and as a result, is interchangeable with objects that have the same interface but a different implementation.

To illustrate the point just above, consider this variation of the `counter` declaration above, of `byteCounter`:

``` motoko
import Nat8 "mo:base/Nat8";
object byteCounter {
  var count : Nat8 = 0;
  public func inc() { count += 1 };
  public func read() : Nat { Nat8.toNat(count) };
  public func bump() : Nat { inc(); read() };
};
```

This object has the same type as the previous one, and thus from the standpoint of type checking, this object is interchangeable with the prior one:

``` motoko no-repl
{
  inc  : () -> () ;
  read : () -> Nat ;
  bump : () -> Nat ;
}
```

This version does not use the same implementation of the counter field. Rather than use an ordinary natural [`Nat`](../base/Nat.md), this version uses a byte-sized natural number, type [`Nat8`](../base/Nat8.md), whose size is always eight bits.

As such, the `inc` operation may fail with an overflow for this object but never the prior one, which may instead fill the program’s memory.

Neither implementation of a counter comes without some complexity. In this case, they share a common type.

The common type abstracts the differences in the implementations of the objects, shielding the rest of the application from their implementation details.

Objects types can also have [subtypes](object-subtyping.md), allowing an object with a more specific type to pass as an object of a more general type, for example, to pass as an object with fewer fields.

## Object and actor classes

**Object classes** : A family of related objects to perform a task with a customizable initial state. Motoko provides a syntactical construct, called a `class` definition, which simplifies building objects of the same type and implementation.

**Actor classes** : An object class that exposes a [service](async-data.md) using asynchronous behavior. The corresponding Motoko construct is an [actor class](actor-classes.md), which follows a similar but distinct design.

## Object classes

In Motoko, an object encapsulates state, and an object `class` is a package of two entities that share a common name.

Consider this example `class` for counters that start at zero:

``` motoko no-repl
class Counter() {
  var c = 0;
  public func inc() : Nat {
    c += 1;
    return c;
  }
};
```

The value of this definition is that we can construct new counters, each starting with their own unique state, initially at zero:

``` motoko no-repl
let c1 = Counter();
let c2 = Counter();
```

Each is independent:

``` motoko no-repl
let x = c1.inc();
let y = c2.inc();
(x, y)
```

You could achieve the same results by writing a function that returns an object:

``` motoko
func Counter() : { inc : () -> Nat } =
  object {
    var c = 0;
    public func inc() : Nat { c += 1; c }
  };
```

Notice the return type of this constructor function is an object type:

``` motoko no-repl
{ inc : () -> Nat }
```

You may want to name this type such as `Counter` for use in further type declarations:

``` motoko no-repl
type Counter = { inc : () -> Nat };
```

The `class` keyword syntax shown above is a shorthand for these two definitions of `Counter`: a factory function `Counter` that constructs objects, and the type `Counter` of these objects. Classes do not provide any new functionality beyond this convenience.

### Class constructor

An object class defines a constructor function that may carry zero or more data arguments and zero or more type arguments.

The `Counter` example above has zero of each. The example below takes two data arguments, `arg1` and `arg2`, with `Type1` and `Type2` as the types of these arguments, respectively.

``` motoko no-repl
class MyClass(arg1: Type1, arg2: Type2) {
  // class body here
};
```

For example, you can write a `Counter` class that takes an argument of type `Nat` and an argument of type `Bool`:

``` motoko no-repl
import Nat "mo:base/Nat";

persistent actor {

  class Counter(init : Nat, flag : Bool) {
    var c = init;
    var f = flag;
    public func inc() : Nat {
      if f {
        c += 1;
      };
      return c;
    };
  };

}
```

The type arguments, if any, parameterize both the type and the constructor function for the class.

The data arguments, if any, parameterize only the constructor function for the class.

#### Data arguments

Suppose you want to initialize the counter with some non-zero value. You can supply that value as a data argument to the `class` constructor:

``` motoko
class Counter(init : Nat) {
  var c = init;
  public func inc() : Nat { c += 1; c };
};
```

This parameter is available to all methods. For instance, you can `reset` the `Counter` to its initial value, a parameter:

``` motoko
class Counter(init : Nat) {
  var c = init;
  public func inc() : Nat { c += 1; c };
  public func reset() { c := init };
};
```

#### Type arguments

Suppose you want the counter to actually carry data that it counts, like a specialized `Buffer`.

When classes use or contain data of arbitrary type, they carry a type argument. This is equivalent to a type parameter for an unknown type, just as with functions.

The scope of this type parameter covers the entire `class` with data parameters. As such, the methods of the class can use these type parameters without reintroducing them.

``` motoko
import Buffer "mo:base/Buffer";

class Counter<X>(init : Buffer.Buffer<X>) {
  var buffer = init.clone();
  public func add(x : X) : Nat {
    buffer.add(x);
    buffer.size()
  };

  public func reset() {
    buffer := init.clone()
  };
};
```

#### Type annotation

The class constructor may also carry a type annotation for its return type. When supplied, Motoko checks that this type annotation is compatible with the body of the class, which is an object definition. This check ensures that each object produced by the constructor meets the supplied specification.

For example, repeat the `Counter` as a buffer and annotate it with a more general type `Accum<X>` that permits adding, but not resetting, the counter. This annotation ensures that the objects are compatible with the type `Accum<X>`.

``` motoko
import Buffer "mo:base/Buffer";

type Accum<X> = { add : X -> Nat };

class Counter<X>(init : Buffer.Buffer<X>) : Accum<X> {
  var buffer = init.clone();
  public func add(x : X) : Nat { buffer.add(x); buffer.size() };
  public func reset() { buffer := init.clone() };
};
```

#### Full syntax

Classes are defined by the keyword `class`, followed by:

- A name for the constructor and type being defined. For example, `Counter`.

- Optional type arguments. For example, omitted, or `<X>`, or `<X, Y>`.

- An argument list. For example, `()`, or `(init : Nat)`, etc.

- An optional type annotation for the constructed objects. For example, omitted, or `Accum<X>`.

- The class "body" is an object definition, parameterized by the type and value arguments, if any.

The constituents of the body marked `public` contribute to the resulting objects' type and these types compared against the optional annotation, if given.

Consider the task of walking the bits of a natural [`Nat`](../base/Nat.md) number. For this example, you could define the following:

``` motoko
class Bits(n : Nat) {
  var state = n;
  public func next() : ?Bool {
    if (state == 0) { return null };
    let prev = state;
    state /= 2;
    ?(state * 2 != prev)
  }
}
```

The above class definition is equivalent to the simultaneous definition of a structural type synonym and a factory function, both named `Bits`:

``` motoko no-repl
type Bits = {next : () -> ?Bool};
func Bits(n : Nat) : Bits = object {
  // class body
};
```


## Object combination and extension

Motoko allows you to construct a single object from a simple record and a more complicated object block, while also providing syntax for building new objects from existing ones, adding new fields, or replacing existing fields.
The *base* records and objects are separated by the `and` keyword and can be followed by `with` and semicolon-separated additional (or overriden) fields.
The bases and fields are enclosed in braces, indicating record formation.
When the bases have overlapping fields (according to their types), then a disambiguating field overwrite must be provided.
The original bases are never modified; instead, their fields are copied to create a new object, and thus we refer to this as a functional object combination and extension.

Here are some simple examples:

1. Object combination with `and`:
   The `and` keyword combines two or more objects.

``` motoko
let person = { name = "Alice"; };
let employee = { id = 123; department = "Engineering" };

let employedPerson = { person and employee };
// employeePerson now has: name, id, and department
```

2. Object extension with `with`:
   The `with` keyword allows you to add new fields or override existing ones.

``` motoko
let person = { name = "Alice" };

let agedPerson = { person with age = 30 };

// agedPersion now has: name and age
```

3. Combining `and` and `with`:
   You can use both `and` and `with` together for more complex object manipulations.

``` motoko
let person = { name = "Alice" };
let employee = { id = 123; department = "Engineering" };

let employedPersonWithAge = { person and employee with age = 30 };
// employedPersionWithAge now has: name, id, department and age
```

Key points to remember:
- When using `and`, if there are conflicting field names in the bases, the conflict must be resolved using a `with` field.
- The `with` clause is used to disambiguate field labels, define new fields, override existing fields, add new `var` fields, or redefine existing `var` fields to prevent aliasing.
- You must explicitly override any `var` fields from base objects to prevent introducing aliases.

This syntax provides a convenient way to create modular and reusable code in Motoko, allowing developers to build complex objects from simpler components and
extend existing objects with new functionality.

For more details, see the [language manual](../reference/language-manual#object-combinationextension).

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />
