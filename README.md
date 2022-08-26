# [Motoko](https://internetcomputer.org/docs/current/developer-docs/build/cdks/motoko-dfinity/motoko/) &middot; [![GitHub license](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0) [![npm version](https://img.shields.io/npm/v/motoko.svg?logo=npm)](https://www.npmjs.com/package/motoko) [![Tests](https://img.shields.io/github/workflow/status/dfinity/motoko/release?logo=github)](https://github.com/dfinity/embed-motoko/actions?query=workflow:"release") [![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](https://github.com/dfinity/motoko/blob/master/Building.md)


A safe, simple, actor-based programming language for authoring [Internet Computer](https://internetcomputer.org/) (IC) canister smart contracts.

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
