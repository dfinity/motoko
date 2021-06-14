# Motoko

A simple language for writing Internet Computer (IC) actors.

## User Documentation & Samples

* [Building, installing, developing on Motoko](Building.md).
* [Overview slides](doc/overview-slides.md).
* [Small samples](samples).
* [Language manual and general documentation](doc/modules/language-guide/pages/language-manual.adoc)
* [Concrete syntax](doc/modules/language-guide/examples/grammar.txt)

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
