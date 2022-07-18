# Using Motoko base modules

The design of Motoko strives to minimize built-in types and operations. Instead of built-in types, Motoko provides a base library of modules to handle many kinds of common operations and make the language feel complete. This base library is still evolving with modules that support core features and all of the base library APIs are subject to change over time to varying degrees. You should note, in particular, that the size and number of modules and functions included in the base library is likely to increase dramatically and updates to the base library modules might introduce breaking changes that require you to update your programs to remain compatible.

## Importing from the base library

To import from the base library, use the `import` keyword followed by a local module name and a URL where the `import` declaration can find the module you want to import. For example:

``` motoko
import Debug "mo:base/Debug";
Debug.print("hello world");
```

This example illustrates how to import Motoko code—indicated by using the `mo:` prefix—using the `base/` base library path and the module name `Debug`. You can also import Motoko code and other modules using relative paths. For example, if you have created a Motoko program named `types.mo` in the same folder as your main program, you could include it with an import declaration like this:

``` motoko no-repl
import Types "./types";
```

## Viewing the base library modules

You can find source code and documentation for Motoko base modules in the [motoko-base](https://github.com/dfinity/motoko-base) open source repository. There are instructions in the repository for generating a local copy of the current documentation for the Motoko base library.

You can also search for documentation by using Search in any page of the Developer Center.
