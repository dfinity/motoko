---
sidebar_position: 2
hide_table_of_contents: true
---

# Imports

In Motoko, related code modules are organized into packages. Modules can be imported either from named packages or from the local file system using relative paths. The compiler locates packages on the file system based on a command line argument specifying their location.

Imports should be placed at the top of the source file. They enable code reuse from external libraries or modules, helping to improve maintainability and organization. You can import from:

**1. Standard modules provided by the base package.**

```motoko no-repl
import Text "mo:base/Text";
import Nat "mo:base/Nat";
import Math "mo:base/Float";
```

The package `base` is Motoko's standard library.
This imports the `Text`, `Nat` and `Float` modules from package `base` under the local names `Text`, `Nat` and `Math`.

While not required, it's considered good practice to import a module using its package-defined name. This helps with consistency and readability across codebases.

**2. Packages installed via a package manager (such as Mops).**

```motoko no-repl
import Iter "mo:base/itertools";
```

The module `Iter` is imported from a third-party package `itertools`.

**3. Files within the current project.**

```motoko no-repl
import Utils "Utils";
```

**You can also import specific functions from a module:**

```motoko no-repl
import { compare } "mo:base/Nat";
```

**You can also import specific types from a module:**

```motoko no-repl
import { type Result; mapOk } "mo:base/Result";
```

Learn more about [modules and imports](https://internetcomputer.org/docs/motoko/fundamentals/basic-syntax/imports).
