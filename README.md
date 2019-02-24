# ActorScript

[![Build Status](https://jenkins.london.dfinity.build/job/actorscript-multibranch/job/master/badge/icon)](https://jenkins.london.dfinity.build/job/actorscript-multibranch/job/master/)

A simple language for writing Dfinity actors.


## Installation using Nix

To install the `asc` binary into your nix environment, use

```
$ nix-env -i -f . -A native
```

## Setup of `dev`

Until we join the monorepo, we need a checkout the `dev` repository in
`nix/dev`; see the `Jenkinsfile` the precise revision to use.

For a fresh checkout, run
```
git clone --recursive git@github.com:dfinity-lab/dev nix/dev
git -C nix/dev checkout 2bc6…see…Jenkinsfile…fecd
git -C nix/dev submodule update --init --recursive
```

To update, just run the last two commands again.


## Development using Nix

This is the command that should always pass on master is the following, which builds everything:
```
$ nix-build
```

To enter a shell with the necessary dependencies, you can use

```
$ nix-shell
```
within this shell you can run `make asc` in `src/` to build the `asc` binary,
and use the test suite in `src/test`.


To build `asc.js`, the JavaScript library, use
```
nix-build -A js
```


By default, `dvm` is built using the V8 engine. To build with the Haskell
engine, pass `--arg v8 false` to any of the above `nix-*` commands.


## Development without Nix

You can get a development environment that is independent of nix (although
installing all required tools without nix is out of scope).

 * Use your system’s package manager to install `ocaml` and
   [`opam`](https://opam.ocaml.org/doc/Install.html)
 * Install the packages listed as `OPAM_PACKAGES` in `src/Makefile`:
   ```
   opam install num vlq yojson bisect_ppx bisect_ppx-ocamlbuild menhir
   ```
 * Install the `wasm` package. We use a newer version than is on opam, and a
   fork that supports the multi-value extension. See `nix/ocaml-wasm.nix` for
   the precise repository and version. You can use `nix` to fetch the correct
   source for you, and run the manual installation inside:
   ```
   cd $(nix-build -Q -A wasm.src)/interpreter
   make install
   ```
 * Install the `wasm` tool, using
   ```
   nix-env -i -f . -A wasm
   ```
 * Install the `dvm` tool, using
   ```
   nix-env -i -f . -A dvm
   ```
   or simply
   ```
   ./update-dvm.sh
   ```
   which also updates the `dev` checkout.


## Create a coverage report

Run

    BISECT_COVERAGE=YES make -C src asc
    make -C test coverage

and open `test/coverage/index.html` in the browser, or run

   nix-build -A coverage-report

and open the path printed on the last line of that command.


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

* Nominally and structurally typed with simple generics and subtyping

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
  - `a + b`

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

* Instance check
  - `x is T`

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

See [here](design/Syntax.md).


## Semantics

TODO...


## Implementation

See [here](design/Implementation.md)
