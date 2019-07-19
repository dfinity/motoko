# ActorScript

A simple language for writing Dfinity actors.

## Introduction

### Motivation and Goals

* High-level language for programming Dfinity applications

* Simple ("K.I.S.S.") design and familiar syntax for average programmers

* Good and convenient support for actor model

* Good fit for underlying Wasm and Dfinity execution model

* Anticipate future extensions to Wasm where possible


### Key Design Points

* Simple class-based OO language, objects as closures

* Classes can be actors

* Async construct for direct-style programming of asynchronous messaging

* Structurally typed with simple generics and subtyping

* Overflow-checked number types, explicit conversions

* JavaScript/TypeScript-style syntax but without the JavaScript madness

* Inspirations from Java, C#, JavaScript, Swift, Pony, ML, Haskell


### Left for Future Version

* Gas-related features?

* Exception handling

* Tail calls

* Mixin composition for inheritance

* Fancier types (co/contra-variance annotations?)

* Linear types?

* Atoms?

* String interpolation?


## Overview

### Types

* Primitive types: integers, naturals, words, floats, characters, (unicode) text, bool, null
  - `Int`, `Nat` (trap on overflow)
  - `Word8`, `Word16`, `Word32`, `Word64` (wrap around)
  - `Int8`, `Int16`, `Int32`, `Int64`, `Nat8`, `Nat16`, `Nat32`, `Nat64` (trap on over/underflow)
  - `Float`
  - `Char`, `Text`
  - `Bool`, `Null`

* Function types: first-class, multiple return values, can be generic
  - `T -> U`
  - `(T, U) -> (V, W)`
  - `(x : T, y : U) -> V`
  - `<A, B>(x : T, y : U) -> (V, W)`

* Object types: structural record types, JS-like, fields can be mutable, can be marked as actor
  - `{x : T; var y : U; z : V}`
  - `actor {f : T -> (); g : U -> async T}`

    The fields of an actor are all of function type with a return type of `async t` or `()`.

* Array types: elements can be mutable or immutable
  - `[T]`
  - `[var T]`

* Tuple types: heterogeneous aggregates of fixed size
  - `(Bool, Float, Text)`

* Option types: ML/Haskell-style option/maybe type, other types do not include null!
  - `? T`

* Async types: like futures/promises
  - `async T`

* Like types: structural expansions of nominal types
  - `like T`

* Structural equi-recursive subtyping

* Generics over reference types, uniform representation

* Distinguish sharable and non-sharable types
  - an object type is non-sharable if it has a mutable field or one of non-sharable type
  - an array is non-sharable if it is mutable or has non-sharable element type
  - a function is non-sharable if has a non-async result or a parameter or result of non-sharable type or closes over non-sharable locals (how indicate the latter in type?)
  - all other types are sharable
  - all public actor functions must be sharable


### Expressions and Statements

* Identifiers
  - `x`, `foo_bar`

* Literals for primitive types
  - `13`, `0xf4`, `-20`, `1_000_000`
  - `3.14`, `-0.3e+15`
  - `'x'`, `'\u{6a}'`
  - `"boo"`, `"foo \u{62}ar"`
  - `true`, `false`
  - `null`

* Unary and binary arithmetic operators
  - `- x`, `not b`
  - `a + b`, `c ** d`

* Object, actor, and array literals, field/element access and update
  - `{c = 3; var x = 4; f() {return y}; private y = 9}`
  - `actor {f() {}; private var x = 4; g() : async Int {return y}}`

     Actor literals have restrictions. In particular, every public field
     * must be a syntactic function with
     * a return type of `async t` or `()` and
     * a body of the form `async  …`.

  - `[3, 4]`
  - `o.x`
  - `a[i]`
  - `tuple.0`

* Function calls, short-cut return
  - `f(x, y)`
  - `f<T, U>(x, y)`
  - `return f()`

* Blocks with local scope
  - `{let x = h(); f(x); g(x)}`

* Conditionals and switches
  - `if b ...`
  - `if b ... else ...`
  - `switch x { case 1 ...; case 2 ...; case _ ...}`

* Short-circuit logical operators
  - `b and c`, `a or d`

* While loops and iterations
  - `while (p()) ...`
  - `loop ...`
  - `loop ... while (p())`a
  - `for x in f() ...`

* Labels, nested break and continue
  - `do l ...`
  - `break l`
  - `continue l`

* Async and await
  - `async {... await f() ...}`

* Type annotation
  - `e : T`

* Assertions
  - `assert (x > 0)`

* Every statement is an expression


### Declarations

* Immutable and mutable variables, with destructuring
  - `let x = f()`
  - `let x : T = f()`
  - `var y : T`
  - `var z = 0`
  - `let (a, b, c) = f()`

* Functions
  - `func f() ...`
  - `func g(x : T, y : U) ...`
  - `func h(x : T, y : U) : V ...`
  - `func p<A, B>(x : T, y : U) : V ...`

* Type aliases
  - `type X = T`
  - `type X<A, B> = U`

* Classes, can be annotated as actor, instantiation as function call
  - `class C(x : T, y : U) {...}`
  - `class D<A, B>(x : T) {...}`
  - `actor class A() {...}`
  - `C(4, 5)`

* Every declaration is a statement (and thereby an expression)

## Example

```
import List "as:std/list";

type Post = shared Text -> ();

actor class Server() = {
  private var clients : List<Client> = List.empty<Client>();

  private func broadcast(msg : Text) {
    for (client in List.iter(clients)) {
      client.send(msg);
    };
  };

  public func subscribe(name : Text, client : Client) : async Post {
    clients := List.cons(client, clients);
    return shared func(msg) { broadcast(name # "> " # msg) };
  };
};


actor class Client(name : Text, server : Server) = this {
  public func go() {
    ignore(async {
      let post = await s.subscribe(name, this);
      post("hello");
      post("goodbye");
    });
  };

  public func send(msg : Text) {
    print(name # " received " # msg # "\n");
  };
};


let server = Server();
let bob = Client("bob", server);
let alice = Client("bob", server);
let charlie = Client("charlie", server);
bob.go();
alice.go();
charlie.go();
```


## Syntax

See [here](design/Syntax.html).


## Semantics

TODO...


## Implementation

See [here](design/Implementation.html)
