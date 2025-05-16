---
sidebar_position: 2
hide_table_of_contents: true
---

# Imports

In Motoko, related code modules are organized into packages. Modules can be imported either from named packages or from the local file system using relative paths. The compiler locates packages on the file system based on a command-line argument specifying their location.

Imports should be placed at the top of the source file. They enable code reuse from external libraries or modules, helping to improve maintainability and organization. You can import from:

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
The module `Iter` is imported from a third-party package `itertools`.
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