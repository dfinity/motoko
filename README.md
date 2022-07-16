# Motoko

A safe and simple, actor-based programming language for authoring [Internet Computer](https://internetcomputer.org/) (IC) canister smart contracts.

## User Documentation & Samples

* [Building, installing, and developing on Motoko](Building.md).
* [Overview](doc/md/overview.md).
* [Small samples](samples).
* [Language manual](doc/md/language-manual.md).
* [Concrete syntax](doc/md/examples/grammar.txt).
* [Documentation sources](doc/md/).
* [Base library documentation](doc/md/base/index.md).

## Introduction

### Motivation and Goals

* High-level language for programming IC applications

* Simple ("K.I.S.S.") design and familiar syntax for average programmers

* Good and convenient support for actor model

* Good fit for underlying Wasm and IC execution model

* Anticipate future extensions to Wasm where possible


### Key Design Points

* Simple class-based OO language, objects as closures

* Classes can be actors

* Async construct for direct-style programming of asynchronous messaging

* Structurally typed with simple generics and subtyping

* Overflow-checked number types, explicit conversions

* JavaScript/TypeScript-style syntax but without the JavaScript madness

* Inspirations from Java, C#, JavaScript, Swift, Pony, ML, Haskell

## Related repositories

* [Base library](https://github.com/dfinity/motoko-base).
* [Vessel package manager](https://github.com/dfinity/vessel).
