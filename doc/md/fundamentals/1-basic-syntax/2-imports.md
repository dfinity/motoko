---
sidebar_position: 2
hide_table_of_contents: true
---

# Imports

Package imports should be at the top of the source file. Imports allow you to reuse code from external libraries or modules, making code easier to maintain and manage. You can import from:

**1. Standard modules provided by the base library.**

```motoko no-repl
import Text "mo:base/Text";
```

**2. Third-party packages installed via the Mops package manager.**

```motoko no-repl
import Package "mo:packagename";
```

**3. Files within the current project.**

```motoko no-repl
import Utils "utils";
```

You can also import specific functions from a module:

```motoko no-repl
import { compare } "mo:base/Nat";
```

Learn more in [Modules and imports](https://internetcomputer.org/docs/motoko/fundamentals/basic-syntax/modules-imports).

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />