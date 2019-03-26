# ActorScript

[![Build Status](https://jenkins.london.dfinity.build/job/actorscript-multibranch/job/master/badge/icon)](https://jenkins.london.dfinity.build/job/actorscript-multibranch/job/master/)

A simple but useful language for writing Dfinity actors.

---

# Introduction

---

## Motivation / Goals

* High-level language for Dfinity dapps
* Simple K.I.S.S. design / familiar syntax
* Incorporating ~~actor~~ *canister* model
* Good fit for Wasm / Dfinity execution 
* Forward looking (anticipating Wasm v.*X*)

---

### Key Design Points

* simple class-based language, objects as closures
* Classes can define actors
* Async construct for sequential programming of asynchronous messaging
* Structural typing
* Simple generics and subtyping
* Unbounded or checked arithmetic, explicit conversions
* JavaScript/TypeScript-style syntax but (really) typed & sane

Inspirations from Java(Script), C#, Swift, Pony, ML, Haskell

---

## Left for Future Version

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


## Overview

---

## Types

---

## Primitive types: integers, naturals, words, floats, characters, (unicode) text, bool, null

*`Int`, `Nat` (trap on overflow)
* `Word8`, `Word16`, `Word32`, `Word64` (wrap around)
* `Char`, `Text`
* `Bool`, `Null`
* (Future: determised `Float`)

---

## Function types:

* first-class, multiple return values, can be generic

  - `T -> U`
  - `(T, U) -> (V, W)`
  - `(x : T, y : U) -> V`
  - `<A, B>(x : T, y : U) -> (V, W)`

---

##  Object types: structural record types, JS-like, fields can be mutable, can be marked as actor

* `{x : T; var y : U; z : V}`
* `actor {f : T -> (); g : U -> async T}`

* The fields of an actor are all of function type with a return type of `async t` or `()`.

---

##  Array types

* `[T]` (immutable, sharable)
* `[var T]` (mutable, local)

---

## Tuple types

* heterogeneous aggregates of fixed size
* immutable fields

* `(Bool, Float, Text)`

---

## Option types

* explicit nullable types 
* ML/Haskell-style option/maybe type
  - `? T`

* other types _*do not include*_ null by default!

---


## Async types

* new type `async T`
* asychronous future (a.k.a. *promises*)
* a handle to future result of type `T`. 
* expression `await <promise>` suspends computation until result.

---

## Type System

* Structural equi-recursive subtyping

* Generics over all types 
  * uniform representation, not specialization

---

## Sharability

*sharable* \~ stateless, serializable

* AS distinguishes sharable and non-sharable types
  - an object type is non-sharable if it has a mutable field or one of non-sharable type
  - an array is non-sharable if it is mutable or has non-sharable element type
  - a function is non-sharable if has a non-async result or a parameter or result of non-sharable type or closes over non-sharable locals (how indicate the latter in type?)
  - all other types are sharable
  - all public actor functions must be sharable

## Expressions and Statements

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

* Unary & binary,  arithmetic & logical operators
  - `- x`, `not b` ...
  - `a + b` ...
  - `a & b` ...

---

## Object, actor, and array literals, field/element access and update
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

## Function calls, short-cut return

* `f(x, y)`
* `f<T, U>(x, y)`
* `return f()`

---

## Blocks with local scope

* `{let x = h(); f(x); g(x)}`

## Conditionals and switches
  - `if b ...`
  - `if b ... else ...`
  - `switch x { case 1 ...; case 2 ...; case _ ...}`

##  While, loops and iteration
  - `while (p()) ...`
  - `loop ...`
  - `loop ... while (p())`
  - `for x in f() ...`

## Labels, nested break and continue
  - `do l ...`
  - `break l`
  - `continue l`

## Async and await
  - `async {... await f() ...}`

## Type annotation
  - `e : T`

## ~~Instance check~~
  - ~~`x is T`~~

## Assertions
  - `assert (x > 0)`

---

## Every statement is an expression

---

## Declarations

* Immutable and mutable variables, with destructuring
  - `let x = f()`
  - `let x : T = f()`
  - `var y : T`
  - `var z = 0`
  - `let (a, b, c) = f()`

---

## (first-class) Functions (lambdas)

- `func f() ...`
- `func g(x : T, y : U) ...`
- `func h(x : T, y : U) : V ...`
- `func p<A, B>(x : T, y : U) : V ...`

---

## (Recursive) Type aliases

 - abbreviations:
  ```
  type Post = shared Text -> ();
  ```
 
 - generic/recursion:
   ```
   type List<T> = ?{head : T; var tail : List<T>};
   ```
 
 - mutual recursion:
   ```
   type Exp = ... Stmt ... ;
   type Stmt = ... Exp ...;
   ```
---


## Classes

Classes as records of members, with private state.

```
 class Counter(x : Int) { 
	private state : Int = x;
    inc() { x += 1; };
	get():Int { x; };
  }
```
Instantiation as function call:

``` 
let c = Counter(666);
c.inc();
let 667 = c.get();
```
## Generic Classes

```
class Dict< K, V > (ord: (K,K)-> Int ) { 
  add(k: K, v: V) { ... };
  find( k: K) : ? V { ... };
};
```

```
let d = Dict<Int,Text> (func(i:Int, j:Int ) = i - j);
d.Add(1,"Alice");
let ? name = d.Find(1);
```

## Actor Declarations


```
actor Server { 
 private shared func post():(){...};
 subscribe(c : Client): async Post { post; };
};
```

```
 let post = await Server.subscribe(this);
 post("hello");
 post("world");
```

## Actor Classes

```
actor class Client() { 
  start(S : Server) {};
  message(m : Text) { ...};
};
```

```
let Alice = Client(); // construction as function call
Alice.start(Server); // async send as function call
```


## Sample App

---

## Implementing *Chat*

* baby example
* one server actor
* multiple clients, each an instance of (actor) class Client.

---

## The server


```


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


## Example: The client class

```
actor class Client() = this {
  // TODO: these should be constructor params once we can compile them
  private var name : Text = "";
  private var server : ?Server  = null;

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

## Syntax

See [here](design/Syntax.md).

## Semantics

* call-by-value (like Java,C,JS,ML unlike Haskell,Nix)
* declarations are locally mutually recursive, provided no *use-before-define.*
* parametric polymorphism
* subtyping as subsumption, not coercion.
* no dynamic casts

## Implementation(s)

* implemented in Ocaml (to leverage `wasm` reference implementation)
* clean reference interpreter (OCaml)
* compiler (wasm)
  * multipass with typed IR in each pass.
  * uniform representation, unboxed arithmetic
  * two-space gc (for now), gc between messages
  * relies on typing for performance
* polymorphism by erasure
* subtyping is the identity (i.e. free)

## Documentation

See [here](design/Implementation.md)
