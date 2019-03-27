% "ActorScript"

# Overview

### Motivation and Goals

A simple but useful language  
for writing DFINITY actors.

* High-level language for Dfinity dapps
* Simple design (K.I.S.S.)
* Familiar syntax
* Incorporating ~~actor~~ *canister* model
* Good fit for Wasm / Dfinity execution 
* Forward looking (anticipating Wasm v.*X*)

### Key Design Points

* Class-based OO, functional & imperative
* Objects as records of members
<!--
* Classes can define actors (records of remote asynchronous functions)
-->
* Async construct for sequential programming of asynchronous messaging
* Structural typing
* Simple generics and subtyping
* safe arithmetic (unbounded and checked),  
  explicit conversions
* JavaScript-style syntax but (really) typed & sane

Inspirations: Java(Script), C#, Swift, Pony, ML, Haskell

### Semantics

* call-by-value  
  (like Java, C, JS, ML; unlike Haskell, Nix)
* declarations are locally mutually recursive
* parametric, bounded polymorphism
* subtyping as subsumption, not coercion.
* no dynamic casts

### Implementation(s)

* implemented in OCaml (leverages `wasm` libary)
* simple reference interpreter
* less simple compiler to WebAssembly
  * multipass with typed IR in each pass.
  * uniform representation, unboxed arithmetic
  * two-space gc (for now), gc between messages
  * relies on typing for performance
* polymorphism by erasure
* subtyping is always the identity (thus free)

### Status

* great team!
* interpreter/compiler up and running via `dvm`.
* compiles multiple files by concatenation  
  (good enough for the Produce Exchange)
* documentation (see [draft](actorscript-guide.pdf), 30 pages)
* had to backpedal for static canisters from dynamic actors

### Backlog

* (Full) Standard library (e.g. Unicode API)
* IDL design/compiler (in ~~dispute~~ progress).
* Upgrade story
* Library mechanism/true separate compilation
* ML-like variant types (e.g. for trees)
* Better memory manager and garbage collector
* Move to bidirectional messaging  
  (blocked on proposal and hypervisor support)
* Change the name?

# The language

### Types


### Primitive types

* `Int`, `Nat` (trap on overflow)
* `Word8`, `Word16`, `Word32`, `Word64` (wrap around)
* `Char`, `Text` (unicode)
* `Bool`, `Null`
* (Future: deterministic `Float`, `Double`)

### Function types

* first-class

* multiple arguments and returns

* possibly generic

  - `T -> U`
  - `(T, U) -> (V, W)`
  - `(x : T, y : U) -> V`
  - `<A, B>(x : T, y : U) -> (V, W)`

###  Object types

structural record types, JS-like, fields can be mutable

* `{var x : Int; color : Color}`

* `shared {x : Int; color: Color}`

shared (think serializable) objects have immutable fields of sharable types.

### Actor types

Like object types, but marked as `actor`:

```
actor {
  f : Text -> ();
  g : Int -> async Int
}
```

The fields of an actor are functions with

* return type `()` (think void) or
* return type `async T` (a promise)

###  Array types

* `[T]` (immutable, sharable)

* `[var T]` (mutable, local)

### Tuple types

`(Bool, Float, Text)`

* heterogeneous aggregates of fixed size
* immutable fields


### Option types

`?T`

* explicit nullable types

* values:
  * `null`  
    or
  * `? x`  
    for a `x : T`.

* (no other type contains `null`)


### Async types

`async T`

* asychronous future (a.k.a. *promises*):  
  a handle to a future value of type `T`.

* introduced by expression `async e`.

* expression `await e` suspends computation pending `e`'s result.

### Type System

* Structural, equi-recursive subtyping  
  (definitions are equations).

* Generics over all types

* Constraints on type parameters (as in Java/C#)

* uniform representation, not specialization

* bidirectional type checking  
  (not ML-style inference)

### Sharability

AS distinguishes sharable types:
 
*sharable* \~ stateless, serializable

*non-sharable* \~ stateful, non-serializable
  
  - all primitive types are sharable (scalars + text)
  - any `shared` function type is sharable
  - any `shared` object type is sharable
  - any `actor` type is sharable
  - `[T]` and `?T`  are sharable if `T` is shareable.
  - all other types are non-sharable
 <!--
  - any parameter `T <: Shared` is sharable.
(subtyping respects `sharability`)
 -->


### Expressions and Statements

* Identifiers: `x`, `foo_bar`, `List`, `Map`

* Literals for primitive types
  - `13`, `0xf4`, `-20`, `1_000_000`
  - `3.14`, `-0.3e+15`
  - `'x'`, `'\u{6a}'`, `'☃'`,
  - `"boo"`, `"foo \u{62}ar ☃"`
  - `true`, `false`
  - `null`

* Unary & binary, arithmetic & logical operators
  - `- x`, `not b`, `a + b`, `a & b` ...

### (Shared) Objects

* `shared` (think serializable) objects have immutable fields of sharable type:

  ```
  shared { x = 0; color = Colors.Red }
  ```

* full `object`s can be mutable, stateful  
  (but not sharable)
  ``` swift
  object {
    private var c = 0;
    inc() {c += 1};
    get() : Int {c}
  }
  ```

### Actors

Actors are restricted objects:

* state must be isolated
* public methods implicitly `shared`
* interface asynchronous

```
actor { 
  private var c = 0;
  inc() {c += 1}; 
  get() : async Int {c}
}
```


### field access and update

* object access/update:

  * `point.color`
  * `point.x += 1`
  * `point.move(dx,dy)`

* actor access/communication:

  *  `client.send(text)`
  *  `await server.subscribe(client)`


<!--
### Actor typing

* Actors are restricted objects, always sharable (by reference).

* All actor state must be private and local (no access to enclosing state).

* Every public actor field must be a 'shared' function, returning
  * an awaitable promise of type `async T` (async request)
  * or nothing `()` (fire & forget)

(`shared` functions are asynchronous with serializable arguments/returns.)
-->

### Tuples

Tuples are anonymous aggregates of fixed length
```
  type Point3D = (Int, Int, Int);
  let origin = (0, 0, 0);
  let (x, y, z) = origin;

  func isOrigin(p : Point3D) : Bool {
    switch p {
      case (0, 0, 0) true; // pattern match
      case _ false;
    }
  }
```

### Arrays



* `[1,2,3,4]` ( immutable, type `[Nat]` )
* `[var 1,2,3,4]` (mutable, type `[var Nat]`)
* `a[i]`, `a[i] += 1`

'nuff said

### Function calls, short-cut return

* Arguments as tuples:

  ```
  compare(x, y)
  ```
* Anonymous functions:

  ```
  let bs = fun_sort<Int>(as, func(a:Int, b:Int) { a - b });
  ```

* We can return early from a function with `return`:
  ```
  let prod(ints : [Int]) : Int {
    let p = 1;
    for (i in ints.vals) {
      if (i == 0) return 0;
      p *= i
    };
    p;
  }
  ```



### Blocks with local scope

`{ let tmp = x; x := y; y := tmp;  }`

* mutually recursive type and value bindings.

### Conditionals and switches

- `if b ...`
- `if b ... else ...`
- `switch x { case 1 ...; case 2 ...; case _ ...}`  

  sequential pattern matching,  
  traps on if no case matches

###  While, loops and iteration

  - `while (p()) ...`
  - `loop ...`
  - `loop ... while (p())`
  - `for (x in f()) ...`

### Labels, break and continue

  - `label l exp`
  - `do l ...`
  - `break l`
  - `continue l`

labels ensure control flow is structured (no gotos)

### Async

`async e`

* spawns an asynchronous computation of `e`  
  (by sending a message to the enclosing actor)

* the async expression immediately returns control  
  (before `e` has finished)

* its value is a *pending* promise (`async T`)

* The promise is *fulfilled* when `e` completes (asynchronously)


### Await

* You cannot synchronously "block" on a promise.

* Instead, `async` expressions have super-powers ...

  ... they (and only they) can `await` promises!

### Await (cont.)

`await e`

* evaluates `e` to a promise
* then suspends the caller of `await` until/unless the promise has a value.

   ```
      async {
         loop {
            let post = await Server.subscribe(...);
            post("hello");
            s.unsubscribe();
         };
      };
   ```

* (implemented by (selective) cps conversion)

### Type annotation

 `e : T`

* not a run-time cast, just a compile-time constraint
* verified and exploited by the type-checker
* switches direction from (partial) type inference to (complete) type checking

### ~~Instance check~~

~~`x is T`~~

(verboten)

### Assertions

`assert (x > 0)`

* `()` on success, or
* `trap` on failure

### Declarations

* Immutable and mutable variables  
  `let x = f()`  (immutable)  
  `let x : T = f()`  
  `var z  = 0` (mutable)  
  `var z : Int = 0`

* ... with pattern matching:  
  `let (x, y, z) = origin` (can't fail)  
  `let 5 = fib(3)` (will trap)  
  `let ? v = dict.find(key)` (could trap)

### Functions

* non-shared, synchronous, generic, pure or impure
* shared (asynchronous)

```
  // vanilla functions
  func fib(n : Int) : Int {...}
  func fun_sort<A>(as : [A], cmp : (A,A)->Int) : [A] { ... }
  func imp_sort<A>(as : [var A], cmp : (A,A)->Int) { ... }

  // shared functions (messaging)
  shared func broadcast(t : Text) {}
  shared func subscribe() : async Post { broadcast }
```
### Type Definitions

* Abbreviations:
  ```
  type Post = shared Text -> ();
  ```
* Generic and/or recursive type definitions:

  ```
  type List<T> = ?{head : T; var tail : List<T>};
  ```

* Mutual recursion too:

  ```
  type Exp = ... Stmt ... ;
  type Stmt = ... Exp ...;
  ```


### Classes

Classes as functions returning objects:
```
 class Counter(init : Int) {
    private var state : Int = init;
    inc() { state += 1; };
    get() : Int { state; };
  }
```

Class instantiation as function call (no `new`):
```
let c = Counter(666);
c.inc();
let 667 = c.get();
```
### Generic Classes

```
class Dict< K, V > (cmp : (K,K)-> Int ) {
  add(k: K, v: V) { ... };
  find(k: K) : ? V { ... };
};
```

```
let d = Dict<Int,Text> (func (i:Int, j:Int) : Int = i - j);
d.add(1,"Alice");
let ? name = d.find(1);
```
### Actor Declarations

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

### Actor Classes

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

### Language prelude

* connects internal primitives with surface syntax (types, operations)
* conversions like `intToWord32`
* side-effecting operations `printInt`
  (tie into execution environment)
* utilities like `hashInt`, `clzWord32`


# Sample App


### Implementing *Chat*

* type example
* one server actor
* multiple clients, each an instance of (actor) class Client.

### Chat Server

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
```

```
  subscribe(client : Client) : async Post {
    let cs = new {head = client; var tail = clients};
    clients := ?cs;
    return broadcast;
  };
};
```


### Example: The client class

```
actor class Client() = this {

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

```

```
  send(msg : Text) {
    print(name # " received " # msg # "\n");
  };
};
```
### Example: test

```
let server = Server();
let bob = Client();
let alice = Client();
let charlie = Client();

bob.go("bob", server);
alice.go("alice", server);
charlie.go("charlie", server);
```

# Produce Exchange

### Produce Exchange

- Example DFINITY app: a marketplace application
  - Participants include:  
    Producers, transporters and retailers
  - Resources: Money, truck routes, produce
  - Other entities: Produce and truck types, regions, reservations

- As a communication tool:  
  Substance: Demonstrate example ActorScript app  
  Process: Document internal development process

- [WIP: Canister in ActorScript](https://github.com/dfinity-lab/actorscript/tree/stdlib-examples/stdlib/examples/produce-exchange)

### Produce Exchange: Define MVP

[**Full MVP def** on Confluence](https://dfinity.atlassian.net/wiki/spaces/DE/pages/116654198/Produce+Exchange+MVP+Product+Requirements)

[**MVP on ActorScript Canister**](https://github.com/dfinity-lab/actorscript/tree/stdlib-examples/stdlib/examples/produce-exchange#produce-exchange-canister-mvp-requirements)

**Summary:**

- defines **users**:  
  Developers, transporters, retailers and producers.
- defines **features** and **use cases**:
  - Resource data can be published and updated
  - Queries require database logic, including joins
- defines non-goals, and out-of-scope goals.

### Produce Exchange: Exit criteria

[**Full details**](https://dfinity.atlassian.net/wiki/spaces/DE/pages/116654198/Produce+Exchange+MVP+Product+Requirements)

**Summary:**

 - People: SDK + ActorScript teams.
 - Feature-based criteria: Same as MVP.
 - Test-based criteria: Automated tests.
 - Operational criteria: Run on DFINITY node.
 - Performance criteria: Run at certain scales / rates.

### [Produce exchange server components](https://github.com/dfinity-lab/actorscript/tree/stdlib-examples/stdlib/examples/produce-exchange#server-components)

- **Server types**: Types for client-server messages
- **Server actor**: Interface for client-server messages
- **Server model types**: Data types used internally
- **Server model implementation**: Implements the actor


### [Standard library](https://github.com/dfinity-lab/actorscript/tree/stdlib-examples/stdlib#actorscript-standard-library)

Why?

- Gather reusable components,  
  (e.g., collections for **server model types**)
- Codify best ActorScript practices

How?

- ActorScript supports some namespace management, and multiple input files.
- [Documentation](https://github.com/dfinity-lab/actorscript/tree/stdlib-examples/stdlib#actorscript-standard-library) generated from the source code


### [Standard library: Produce exchange](https://github.com/dfinity-lab/actorscript/tree/stdlib-examples/stdlib#produce-exchange)

We focus on abstractions for implementing the database for the produce exchange:

- [Document Table](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/design/stdlib/docTable.md): Mutable collection of immutable documents.

- [Hash trie](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/design/stdlib/trie.md): Immutable finite map representation based on hashing each key.

- [Association list](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/design/stdlib/assocList.md): Immutable finite map representation based on a list of key-value pairs.

# (not yet) The End
