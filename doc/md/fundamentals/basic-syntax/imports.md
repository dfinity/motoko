---
sidebar_position: 2
---

# Imports  

Package imports should be at the top of the source file. You can import from:

1. Standard modules provided by the base library:

  ```motoko
  import Text "mo:base/Text";
  ```  

2. Third-party packages installed via the Mops package manager:

  ```motoko
  import Package "mo:packagename";
  ```  

Files within your own project.  

  ```motoko
  import Utils "utils";
  ```  

You can also import specific functions from a module:  

```motoko
import { compare } "mo:base/Nat";
```
