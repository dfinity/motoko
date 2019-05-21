# ActorScript

A simple language for writing Dfinity actors.


## Installation using Nix

To install the `asc` binary into your nix environment, use

```
$ nix-env -i -f . -A native
```

## Development using Nix

This is the command that should always pass on master is the following, which builds everything:
```
$ nix-build
```

To enter a shell with the necessary dependencies, you can use
```
$ nix-shell
```
within this shell you can run
 * `make asc` in `src/` to build the `asc` binary,
 * `make` in `rts/` to build the ActorScript runtime
 * `make` in `test/` to run the test suite.

### Merlin

> For Merlin to be able to give you the type of identifiers or to offer completion from other file of your projects, it needs to know where to find the cmi files of the other modules of your project. 

To do this, run `make -C src asc` within a nix shell.

## Development without Nix

You can get a development environment that is independent of nix (although
installing all required tools without nix is out of scope).

 * Use your system’s package manager to install `ocaml` and
   [`opam`](https://opam.ocaml.org/doc/Install.html)
 * Install the packages listed as `OPAM_PACKAGES` in `src/Makefile`:
   ```
   opam install num vlq yojson bisect_ppx bisect_ppx-ocamlbuild menhir
   ```
 * Install the `wasm` Ocaml package. We use a newer version than is on opam, and a
   fork that supports the multi-value extension. See `nix/ocaml-wasm.nix` for
   the precise repository and version. You can use `nix` to fetch the correct
   source for you, and run the manual installation inside:
   ```
   cd $(nix-build -Q -A wasm.src)/interpreter
   make install
   ```
 * Install various command line tools used by, in particuar, the test suite:
   ```
   nix-env -i -f . -A wasm
   nix-env -i -f . -A filecheck
   nix-env -i -f . -A wabt
   nix-env -i -f . -A dvm
   ```
 * Building the ActorScript runtime without nix is tricky. But you can run
   ```
   nix-shell --run 'make -C rts'
   ```
   to get `rts/as-rts.wasm`.


## Create a coverage report

Three ways of obtaining the coverage report:

 * Run
   ```
   BISECT_COVERAGE=YES make -C src asc
   make -C test coverage
   ```
   and open `test/coverage/index.html` in the browser.

 * Alternatively, you can run
   ```
   nix-build -A coverage-report
   ```
   and open the path printed on the last line of that command.
 * On the VPN, simply go to
   <https://hydra.oregon.dfinity.build//job/dfinity-ci-build/actorscript/coverage-report/latest/download/1/coverage/index.html>
   for the report for the latest version on `master`.

## Profile the compiler

1. Build with profiling
   ```
   make -C src clean
   make BUILD=p.native -C src asc
   ```
2. Run `asc` as normal, e.g.
   ```
   ./src/asc -c foo.as -o foo.wasm
   ```
   this should dump a `gmon.out` file in the current directory.
3. Create the report, e.g. using
   ```
   gprof --graph src/asc
   ```
   (Note that you have to _run_ this in the directory with `gmon.out`, but
   _pass_ it the path to the binary.)


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

* Infinite-precision integers

* Richer destructuring and pattern matching

* Exception handling

* Tail calls

* Mixin composition for inheritance

* Fancier types (generic bounds, top type?, union types?, co/contra-variance?)

* Linear types?

* Atoms?

* String interpolation?


## Overview

### Types

* Primitive types: integers, naturals, words, floats, characters, (unicode) text, bool, null
  - `Int`, `Nat` (trap on overflow)
  - `Word8`, `Word16`, `Word32`, `Word64` (wrap around)
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
type List<T> = ?{head : T; var tail : List<T>};

type Post = shared Text -> ();

actor class Server() = {
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


let server = Server();
let bob = Client();
let alice = Client();
let charlie = Client();
bob.go("bob", server);
alice.go("alice", server);
charlie.go("charlie", server);
```


## Syntax

See [here](design/Syntax.html).


## Semantics

TODO...


## Implementation

See [here](design/Implementation.html)
