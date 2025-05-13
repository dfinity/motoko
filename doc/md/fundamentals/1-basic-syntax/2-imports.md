---
sidebar_position: 2
hide_table_of_contents: true
---

# Imports

In Motoko, libraries of related code modules are collected into packages. A module can be imported from a named package or the local file system using a relative path. The location of a package on the file system is determined by a command line argument to the compiler.

Imports should be at the top of the source file. Imports allow you to reuse code from external libraries or modules, making code easier to maintain and manage. You can import from:

**1. Standard modules provided by the base library.**

```motoko no-repl
import Text "mo:base/Text";
```

**2. Packages installed via a package manager (such as Mops).**

```motoko no-repl
import Nat "mo:base/Nat";
import Math "mo:base/Float";
import Iter "mo:itertools/Iter";
```

This imports the `Nat` and `Float` modules from package `base` under the locals name `Nat` and `Math` (`base` is Motoko's standard library).
Module `Iter` is imported from a third-party package `itertools`.
**3. Files within the current project.**

```motoko no-repl
import Utils "Utils";
```

**You can also import specific functions from a module:**

```motoko no-repl
import { compare } "mo:base/Nat";
```

Other forms of imports can be used to reference actor classes and other actors.

Learn more about [modules and imports](https://internetcomputer.org/docs/motoko/fundamentals/basic-syntax/modules-imports).

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />