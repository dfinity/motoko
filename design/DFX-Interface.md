Stable CLI for dfx
==================

An important way of using the Motoko compiler is via the the `dfx` tool,
provided by the DFINITY SDK, which provides project and package management
support.

This document describes the interface that `moc` and related tools provide to
`dfx`. The goal is that
 * the Motoko developers know which command line flags have to
   be kept stable in order to not break `dfx`, and that
 * the SDK developers have a single place to read about the moc interface, and
   a place to express additional requirements (by collaborating on a PR against
   this document.)

This interface includes:
 * nix derivations imported by SDK
 * binaries executed
 * command line arguments and environment varialbes passed to these binaries
 * where these binaries read files and
 * where these binaries write files, output or temporary
 * where they do _not_ write to, so that upgrading `moc` doesn’t suddenly leave
   artifacts where `dfx` does not expect them

It does not replace proper documentation, but should be kept rather concise.

Nix derivations
---------------

The `motoko` repository defines the following nix derivations, as attributes of
the top-level `default.nix`:

* `moc-bin`: contains `bin/moc`
* `mo-ide`: contains `bin/mo-ide`
* `didc`: contains `bin/didc`
* `rts`: contains `rts/mo-rts.wasm`, the Motoko runtime system
* `stdlib`: contains the standard library, directly in the top level directory,
  as `*.mo` files. It does not contain extra files (test files, for example)

The `default.nix` file itself takes an optional `system` parameter which is
either `"x86_64-linux"` or `"x86_64-darwin"`, and defaults to
`builtins.currentSystem`.

All binaries are either built statically (Linux) or only use system libraries (OSX).

Compiling Motoko Files to Wasm
------------------------------

In order to compile a motoko file, `dfx` invokes `moc` with

    moc some/path/input.mo -o another/path/output.wasm { --package pkgname pkgpath }

in an environment where `MOC_RTS` points to the location of the Motoko runtime system.

This _reads_ `some/path/input.mo` and any `.mo` file referenced by
`some/path/input.mo`, either relatively, absolutely or via the provided package aliases. It also reads the given `mo-rts.wasm` file.

No constraints are imposed where these imported files reside (this may be refined to prevent relative imports from looking outside the project and the declared packages)

This _writes_ to `another/path/output.wasm`, but has no other effect. It does
not create `another/path/`.

Compiling Motoko Files to IDL
-----------------------------

As the previous point, but passing `--idl` to `moc`.

Listing dependencies
--------------------

**WARNING**: This is not yet implemented.

The command

    moc --print-deps some/path/input.mo

prints to the standard output all URLs _directly_ imported by
`some/path/input.mo`, one per line, e.g.

   mo:stdlib/List
   mo:other_package/Some/Module


This _reads_ only `some/path/input.mo`, and writes no files.

By transitively exploring the dependency graph using this command (and
resolving URLs appropriately before passing them as files to `moc`), one can
determine the full set of set of `.mo` files read by the two compilation modes
described above (to wasm and to IDL).


Compiling IDL Files to JS
-------------------------

In order to compile a IDL file, `dfx` invokes `didc` with

    didc --js some/path/input.did -o another/path/output.js

This _reads_ `some/path/input.did` and any `.did` file referenced by
`some/path/input.did`.

No constraints are imposed where these imported files reside (this may be refined to prevent relative imports from looking outside the project and the declared packages)

This _writes_ to `another/path/output.js`, but has no other effect. It does
not create `another/path/`.

Invoking the IDE
----------------

In order to start the language server, `dfx` invokes

    mo-ide --canister-main some/path/main.mo { --package pkgname pkgpath }

with `stdin` and `stdout` connected to the LSP client.


This _reads_ `some/path/input.mo` and any `.mo` file referenced by
`some/path/input.mo`, either relatively, absolutely or via the provided package aliases.

No constraints are imposed where these imported files reside (this may be refined to prevent relative imports from looking outside the project and the declared packages)
