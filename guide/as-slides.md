# ActorScript

[![Build Status](https://jenkins.london.dfinity.build/job/actorscript-multibranch/job/master/badge/icon)](https://jenkins.london.dfinity.build/job/actorscript-multibranch/job/master/)

A simple but useful language for writing Dfinity actors.

---

# Introduction

---

# Motivation and Goals

* High-level language for programming Dfinity applications
* Simple ("K.I.S.S.") design and familiar syntax for average programmers
* Good and convenient support for actor model
* Good fit for underlying Wasm and Dfinity execution model
* Anticipate future extensions to Wasm where possible

---

# Key Design Points

* Simple class-based OO language, objects as closures
* Classes can be actors
* Async construct for direct-style programming of asynchronous messaging
* Nominally and structurally typed with simple generics and subtyping
* Overflow-checked number types, explicit conversions
* JavaScript/TypeScript-style syntax but without the JavaScript madness
* Inspirations from Java, C#, JavaScript, Swift, Pony, ML, Haskell

---

# Left for Future Version

* Gas-related features?
* Infinite-precision integers
* Richer destructuring and pattern matching
* Exception handling
* Tail calls
* Mixin composition for inheritance
* Fancier types (generic bounds, top type?, union types?, co/contra-variance?)
* Linear types?
* Atoms?
* String interpolation?

---


# Overview

---

# Types

---

# Primitive types: integers, naturals, words, floats, characters, (unicode) text, bool, null

*`Int`, `Nat` (trap on overflow)
* `Word8`, `Word16`, `Word32`, `Word64` (wrap around)
* `Char`, `Text`
* `Bool`, `Null`
* (Future: `Float`)

---

# Function types:

* first-class, multiple return values, can be generic

  - `T -> U`
  - `(T, U) -> (V, W)`
  - `(x : T, y : U) -> V`
  - `<A, B>(x : T, y : U) -> (V, W)`

---

#  Object types: structural record types, JS-like, fields can be mutable, can be marked as actor
* `{x : T; var y : U; z : V}`
* `actor {f : T -> (); g : U -> async T}`

* The fields of an actor are all of function type with a return type of `async t` or `()`.

---

#  Array types

* `[T]` (immutable, sharable)
* `[var T]` (mutable, local)

---

# Tuple types

* heterogeneous aggregates of fixed size
* immutable fields

* `(Bool, Float, Text)`

---

* Option types:

* explict nullable types (~~default null~~)
* ML/Haskell-style option/maybe type
  - `? T`

* other types do not include null by default!

---

* Async types: like futures/promises
  - `async T`

* Like types: structural expansions of nominal types
  - `like T`

---

# Type System

* Structural equi-recursive subtyping

* Generics over all types, uniform representation

---

# Sharability

*def.* *sharable* \~ stateless, transmittable

* AS distinguishes between sharable and non-sharable types
  - an object type is non-sharable if it has a mutable field or one of non-sharable type
  - an array is non-sharable if it is mutable or has non-sharable element type
  - a function is non-sharable if has a non-async result or a parameter or result of non-sharable type or closes over non-sharable locals (how indicate the latter in type?)
  - all other types are sharable
  - all public actor functions must be sharable

# Expressions and Statements

* Identifiers
  - `x`, `foo_bar`
  - `List`, `Map`

* Literals for primitive types
  - `13`, `0xf4`, `-20`, `1_000_000`
  - `3.14`, `-0.3e+15`
  - `'x'`, `'\u{6a}'`
  - `"boo"`, `"foo \u{62}ar"`
  - `true`, `false`
  - `null`

* Unary and binary arithmetic and logical operators
  - `- x`, `not b` ...
  - `a + b` ...
  - `a & b` ...

---

# Object, actor, and array literals, field/element access and update
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

---

# Function calls, short-cut return

* `f(x, y)`
* `f<T, U>(x, y)`
* `return f()`

---

# Blocks with local scope

* `{let x = h(); f(x); g(x)}`

* Conditionals and switches
  - `if b ...`
  - `if b ... else ...`
  - `switch x { case 1 ...; case 2 ...; case _ ...}`

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

* ~~Instance check~~
  - ~~`x is T`~~

* Assertions
  - `assert (x > 0)`

* Every statement is an expression

---

# Declarations

* Immutable and mutable variables, with destructuring
  - `let x = f()`
  - `let x : T = f()`
  - `var y : T`
  - `var z = 0`
  - `let (a, b, c) = f()`

---

# (first-class) Functions (lambdas)

- `func f() ...`
- `func g(x : T, y : U) ...`
- `func h(x : T, y : U) : V ...`
- `func p<A, B>(x : T, y : U) : V ...`

---

* (Recursive) Type aliases
  - `type X = T`
  - `type X<A, B> = U`

---

* Classes, can be annotated as actor, instantiation as function call
  - `class C(x : T, y : U) {...}`
  - `class D<A, B>(x : T) {...}`
  - `actor class A() {...}`
  - `C(4, 5)`

---

* Every declaration is a statement (and thereby an expression)

# Sample App

---

# Implementing *Chat*

* baby example
* one server actor
* multiple clients, each an instance of (actor) class Client.

---

# The server


```
type List<T> = ?{head : T; var tail : List<T>};

actor class Server() = {

  type Post = shared Text -> ();

  private var clients : List<Client> = null;

  private shared broadcast(message : Text) {
    var next = clients;
    loop {
      switch next {
        case null return;
        case (?l) {
          l.head.send(message);
          next := l.tail;
        };
      };
    };
  };

  subscribe(client : Client) : async Post {
    let cs = new {head = client; var tail = clients};
    clients := ?cs;
    return broadcast;
  };
};
```

---


# Example: The client class

```
actor class Client() = this {
  // TODO: these should be constructor params once we can compile them
  private var name : Text = "";
  private var server : ?Server  = null;
o
  go(n : Text, s : Server) {
    name := n;
    server := ?s;
    ignore(async {
      let post = await s.subscribe(this);
      post("hello from " # name);
      post("goodbye from " # name);
    });
  };

  send(msg : Text) {
    print(name # " received " # msg # "\n");
  };
};
```
---

# Example: test

```
let server = Server();
let bob = Client();
let alice = Client();
let charlie = Client();

bob.go("bob", server);
alice.go("alice", server);
charlie.go("charlie", server);
```

---

# Syntax

See [here](design/Syntax.md).

# Semantics

* call-by-value (like Java,C,JS,ML unlike Haskell,Nix)
* declarations are locally mutually recursive, provided no *use-before-define.*
* parametric polymorphism
* subtyping as subsumption, not coercion.
* no dynamic casts

# Implementation(s)

* implemented in Ocaml (to leverage `wasm` reference implementation)
* interpreter (OCaml)
* compiler (wasm)
* polymorphism by erasure
* subtyping is free (the identity)

See [here](design/Implementation.md)






-----------------

# Produce Exchange

- Example DFINITY Dapp, as a marketplace application
  - Participants include: Producers, transporters and retailers
  - Resources: Money, truck routes, produce items
  - Other entities: Produce and truck types, regions, reservations

- As a communication tool:
  - Substance: Demonstrate an example Dapp in ActorScript
  - Process: Document our internal development process

- [WIP: Canister in ActorScript](https://github.com/dfinity-lab/actorscript/tree/stdlib-examples/stdlib/examples/produce-exchange).

-----------------

# Produce Exchange: Define MVP

[**Full MVP def** on Confluence](https://dfinity.atlassian.net/wiki/spaces/DE/pages/116654198/Produce+Exchange+MVP+Product+Requirements)

[**MVP on ActorScript Canister**](https://github.com/dfinity-lab/actorscript/tree/stdlib-examples/stdlib/examples/produce-exchange#produce-exchange-canister-mvp-requirements)

**Summary:**
- defines **users**: Developers, transporters, retailers and producers.
- defines **features** and **use cases**:
  - Resource information can be published and updated
  - Queries require database logic, including joins
- defines non-goals, and out-of-scope goals.

-----------------

# Produce Exchange: Exit criteria

[**Full details**](https://dfinity.atlassian.net/wiki/spaces/DE/pages/116654198/Produce+Exchange+MVP+Product+Requirements)

**Summary:**

 - People: SDK + ActorScript teams.
 - Feature-based criteria: Same as MVP.
 - Test-based criteria: Automated tests.
 - Operational criteria: Run on DFINITY node.
 - Performance criteria: Run at certain scales / rates.

-----------------

# [Produce exchange server components](https://github.com/dfinity-lab/actorscript/tree/stdlib-examples/stdlib/examples/produce-exchange#server-components)

- **Server types**: Data types for client-server messages
- **Server actor**: Interface for client-server messages
- **Server model types**: Data types used internally
- **Server model implementation**: Implements the actor


-----------------

# [Standard library](https://github.com/dfinity-lab/actorscript/tree/stdlib-examples/stdlib#actorscript-standard-library)

Why?

- Gather reusable components,

- E.g., the collections for **server model types** component.

- Codify best practices for developing canisters in ActorScript.

How?

- ActorScript supports some namespace management, and multiple input files.

- We generate [documentation](https://github.com/dfinity-lab/actorscript/tree/stdlib-examples/stdlib#actorscript-standard-library) from the ActorScript source code and certain comment blocks it contains.


-----------------

# [Standard library: Produce exchange](https://github.com/dfinity-lab/actorscript/tree/stdlib-examples/stdlib#produce-exchange)

We focus on abstractions for implementing the database for the produce exchange:

- [Document Table](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/design/stdlib/docTable.md): Mutable collection of immutable documents.

- [Hash trie](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/design/stdlib/trie.md): Immutable finite map representation based on hashing each key.

- [Association list](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/design/stdlib/assocList.md): Immutable finite map representation based on a list of key-value pairs.
