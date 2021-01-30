# Motoko

A simple language for writing Dfinity actors.

## User Documentation & Samples

* [Building, installing, developing on Motoko](Building.md).
* [Overview slides](https://hydra.dfinity.systems/job/dfinity-ci-build/motoko/overview-slides/latest/download/1/overview-slides.html) ([sources](doc/overview-slides.md)).
* [Small samples](samples).
* [Language manual](doc/modules/language-guide/pages/language-manual.adoc)
* [Concrete syntax](doc/modules/language-guide/pages/grammar.txt)
* [General documentation](doc/modules/language-guide/pages)

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
