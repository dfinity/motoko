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

    moc some/path/input.mo            \
        -o another/path/output.wasm   \
	{ --package pkgname pkgpath } \
      { --actor-alias alias id }
	[ --actor-idl actorpath ]

in an environment where `MOC_RTS` points to the location of the Motoko runtime system.

This _reads_ the follwing files
 * `some/path/input.mo`
 * any `.mo` file referenced by `some/path/input.mo`, either relatively, absolutely or via the provided package aliases
 * for every actor import `ic:canisteridoralias` imported by any of the Motoko files, it reads `actorpath/canisteridoralias.mo`, see section Resolving Canister Ids below.
 * the given `mo-rts.wasm` file.

No constraints are imposed where imported files reside (this may be refined to prevent relative imports from looking outside the project and the declared packages)

This _writes_ to `another/path/output.wasm`, but has no other effect. It does
not create `another/path/`.

Compiling Motoko Files to IDL
-----------------------------

As the previous point, but passing `--idl` to `moc`.


Resolving Canister aliases
--------------------------

For every actor imported using `import "ic:alias"`, the Motoko compiler treats that as `import "ic:caniserid"`, if the command line flag `--actor-alias alias id` is given.

The relation defined by the set of `--actor-alias` arguments must be left-unique and have disjoint range and domain (i.e. no `--actor-alias a b --actor-alias a c` or `--actor-alias a b --actor-alias b c` or even `--actor-alias a a`).

It is up to `dfx` to determine which urls are aliases that need resolving and which are concrete ids, and to set up `--actor-alias` flags accordingly.

After applying any aliases, the Motoko compiler assume these imports to refer to the [textual representation] of principal ids (e.g. `ic:ABCDE01A7`), and compilation will fail if they are not.

[textual representation]: https://docs.dfinity.systems/spec/public/#textual-ids

Resolving Canister types
------------------------

For every actor imports using `import "ic:canisterid"` (or `import "ic:canisteralias"` if `canisteralias` resolves to `canisterid` as described above), the motoko compiler assumes the presence of a file `canisterid.did` in the actor idl path specified by `--actor-idl`. This files informs motoko about the interface of that canister, e.g. the output of `moc --idl` for a locally known canister, or the IDL file as fetched from the Internet Computer.

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

    mo-ide --canister-main some/path/main.mo \
	{ --package pkgname pkgpath }        \
      { --actor-alias alias id }
	[ --actor-idl actorpath ]

with `stdin` and `stdout` connected to the LSP client.


This _reads_ `some/path/input.mo` and any `.mo` file referenced by
`some/path/input.mo`, either relatively, absolutely or via the provided package aliases.

No constraints are imposed where these imported files reside (this may be refined to prevent relative imports from looking outside the project and the declared packages)
