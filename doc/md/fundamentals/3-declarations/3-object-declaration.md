---
sidebar_position: 3
---

<!-- Some of the leading material might be better under object expressions, if we have a dedicated sections for that -->

# Object declarations

In Motoko, records and objects are both used to group related values using named fields.
Record and objects have the same types, but differ in the way they are created and used.


Record expressions are used to construct simple data structures that consist of named fields holding values. The fields can be mutable or immutable.
The fields of a record cannot refer to each other by name and are mainly used to store plain data, like the records in a database.

Record values can be named using a `let`. That is the only way to declare a named record.

```motoko name=record no-repl
let Motoko = {
  name : Text = "Motoko";
  var age : Nat = 25;
};
```

All fields of a record are accessible.

Record fields can be accessed by the [dot notation](https://en.wikipedia.org/wiki/Object-oriented_programming):

```motoko _include=record no-repl
Motoko.age += 1; // Updates age
Motoko.name; // "Motoko"
```

or by pattern matching on the structure of the record:

```motoko _include=record no-repl
let {name} = Motoko;
name;
```

Object expressions are more general than records. An object is constructed using the `object` keyword followed by a block of field declarations.
Each field declaration is a visibility modifier, `public` or `private` (the default), followed by a declaration.
The public declarations become accessible as fields of the object, while the private field declarations are hidden - private fields do not even appear in the type of the object.
These field declarations can refer to each other in ways that record fields cannot.

The types of record and objects are both described using object types, unordered sequences of named fields describing the content and mutability of each public field.

Both record and object fields can be accessed by either the dot notation or by pattern matching on the names of the fields.

While records expressions are ideal for lightweight data representation, objects expressions are more verbose.
The advantage of object expressions is that they can define full objects in the sense of object-oriented programming: in the object-oriented sense, an object is
a collection of named fields and methods acting on private state.
In Motoko, the private declarations define the encapsulated state, while the public definitions define the object's visible members.


## Declaring an object using `let`

In Motoko, the primitive form of object declaration uses a `let` to name an object expression.:

For example, an object might represent a Motoko user profile:

```motoko no-repl
let Motoko = object {
  public let name = "Motoko";
  private var age = 6;

  public func greet() : Text {
    "Hello, " # name;
  };
  public func birthday() : Text{
    age += 1;
    "It's my birthday, I'm now " # debug_show(age) # " years old!"
  }
};
```

This defines an object with three public members, the field `name` and the methods `greet()` and `birthday()`.
The mutable field `age` is private, ensuring it can only be increased and read with calls to `birthday()`.

## Declaring an object using an object declaration.

Motoko also supports object _declarations_, which stress the definition of an object by using the `object` keyword in place of `let`:

```motoko name=Object no-repl
object Motoko = {
  public let name = "Motoko";
  var age = 6;

  public func greet() : Text {
    "Hello, " # name;
  };
  public func birthday() : Text{
    age += 1;
    "It's my birthday, I'm now " # debug_show(age) # " years old!"
  }
};
```

Since `private` is the default accessibility for the declarations inside an object, the second form is just an abbrevation - syntactic sugar - for the previous `let`-declared object.

### Accessing object fields

Fields and methods of an object are accessed using the [dot notation](https://en.wikipedia.org/wiki/Object-oriented_programming).

```motoko _include=Object no-repl
Motoko.greet() # " " # Motoko.birthday();
```

Or by pattern matching:

```motoko _include=Object no-repl
let {greet=g; birthday} = Motoko;
g() # " " # birthday();
```

The field `age` is private and can't accessed by the dot notation or pattern matching.


## Comparing records and objects

| Feature | Record | Object |
|---------|--------|--------|
| Named fields | Yes | Yes |
| Mutable fields | Yes | Yes |
| Private fields | No | Yes |
| Recursively defined fields | No | Yes |
| Combination (`and`, `with`) | Yes | Yes |

Record and object both use the `var` keyword to define [mutable](https://internetcomputer.org/docs/motoko/fundamentals/declarations/variable-declarations)
and declarations.

Both records and objects support `and` and `with` for merging and updating object fields to create new records and objects.

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />

