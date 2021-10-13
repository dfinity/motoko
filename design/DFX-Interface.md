Stable CLI for dfx
==================

An important way of using the Motoko compiler is via the the `dfx` tool,
provided by the DFINITY SDK, which provides project and package management
support.

This document describes the interface that `moc` and related tools provide to
`dfx`. The goal is that
 * the Motoko developers know which command line flags have to
   be kept stable in order to not break `dfx`, and that
 * the SDK developers have a single place to read about the `moc` interface, and
   a place to express additional requirements (by collaborating on a PR against
   this document.)

This interface includes:
 * nix derivations imported by SDK
 * binaries executed
 * command line arguments and environment variables passed to these binaries
 * where these binaries read files and
 * where these binaries write files, output or temporary
 * where they do _not_ write to, so that upgrading `moc` doesn’t suddenly leave
   artifacts where `dfx` does not expect them

It does not replace proper documentation, but should be kept rather concise.

Nix derivations
---------------

The `motoko` repository defines the following nix derivations, as attributes of
the top-level `default.nix`:

* `moc`: contains `bin/moc`
* `mo-ide`: contains `bin/mo-ide`
* `mo-doc`: contains `bin/mo-doc`
* `didc`: contains `bin/didc`
* `base-src`: contains the base library, directly in the top level directory,
  as `*.mo` files. It does not contain extra files (test files, for example)


The `default.nix` file itself takes an optional `system` parameter which is
either `"x86_64-linux"` or `"x86_64-darwin"`, and defaults to
`builtins.currentSystem`.

All binaries are either built statically (Linux) or only use system libraries (macOS).

Compiling Motoko Files to Wasm
------------------------------

In order to compile a Motoko file, `dfx` invokes `moc` with

    moc some/path/input.mo            \
        -o another/path/output.wasm   \
        { --package pkgname pkgpath } \
        { --actor-alias alias url }
        [ --actor-idl actorpath ]

This _reads_ the following files
 * `some/path/input.mo`
 * any `.mo` file referenced by `some/path/input.mo`, either relatively, absolutely or via the provided package aliases
 * for every actor import `ic:⟨canisterid⟩` imported by any of the Motoko files, it reads `actorpath/⟨canisterid⟩.did`, see section “Resolving Canister aliases” below. Here `⟨canisterid⟩` is the textual representation (e.g. `em77e-bvlzu-aq`).

The package name `prim` is special and should not be set using `--package`.

No constraints are imposed where imported files reside (this may be refined to prevent relative imports from looking outside the project and the declared packages)

This _writes_ to `another/path/output.wasm`, but has no other effect. It does
not create `another/path/`.

Compiler warnings and errors are reported to `stderr`. Nothing writes to `stdout`.

Resolving Canister aliases
--------------------------

For every actor imported using `import "canister:alias"`, the Motoko compiler treats that as `import "ic:⟨canisterid⟩"`, if the command line flag `--actor-alias alias ⟨canisterid⟩` is given. Here, `⟨canisterid⟩` is the textual representation (e.g. `em77e-bvlzu-aq`).

The first argument to `--actor-alias` is the alias without the URL scheme. The second argument must be a [textual representation] of the canister id.

The given aliases must be unique (i.e. no `--actor-alias a aaaaa-aa --actor-alias a em77e-bvlzu-aq`).

[textual representation]: https://sdk.dfinity.org/docs/interface-spec/index.html#textual-ids

Resolving Canister types
------------------------

For every actor imported using `import "ic:⟨canisterid⟩"` (or `import "canister:alias"` if `alias` resolves to `ic:⟨canisterid⟩` as described above), the Motoko compiler assumes the presence of a file `⟨canisterid⟩.did` in the actor idl path specified by `--actor-idl`. This file informs Motoko about the interface of that canister, e.g. the output of `moc --idl` for a locally known canister, or the IDL file as fetched from the Internet Computer.

The `⟨canisterid⟩` here refers to the “textual representation“, e.g. `em77e-bvlzu-aq`.

This file informs Motoko about the interface of that canister. It could be the output of `moc --idl` for a locally known canister, or the IDL file as fetched from the Internet Computer, or created any other way.

Open problem: how to resolve mutual canister imports.

Exporting Canister Metadata
---------------------------

The compiler generates various metadata about the canister via command line flags.
The compiled Wasm module also includes the metadata in the custom sections.

* Candid interface.
  + Compiler flag `--idl` generates the Candid interface for the canister. The main service
    is always a service constructor, which contains the initialization arguments for installing the canister.
  + Custom section `icp:public candid` stores the interface for the running (initialized) canister, which removes
    the initializatio arguments. This custom section is publicly accessible.
  + Custom section `icp:private init_args` stores the initialization arguments. The argument types can refer to
    types defined in the `icp:public candid` custom section. This custom section is only accessible by the controllers
    of the canister.
* Stable variable.
  + Compiler flag `--print-stable-vars` generates the signatures for stable variables.
  + Custom section `icp:private stable_vars` stores the signatures for stable variables. This custom section is
    only accessible by the controllers of the canister.

We store the metadata in the Wasm module, so that dfx can compare the current and new type signatures to ensure
canister upgrade is safe.

Invoking the IDE
----------------

In order to start the language server, `dfx` invokes

    mo-ide --canister-main some/path/main.mo \
        { --package pkgname pkgpath }        \
        { --actor-alias alias url }
        [ --actor-idl actorpath ]

with `stdin` and `stdout` connected to the LSP client.

This may _read_ the same files as `moc` would.

Listing dependencies
--------------------

The command

    moc --print-deps some/path/input.mo

prints to the standard output all URLs _directly_ imported by
`some/path/input.mo`, one per line. Each line outputs the original
URL, and optionally a full path if `moc` can resolve the URL, separated by a space.
For example,

    mo:base/List
    mo:other_package/Some/Module
    ic:em77e-bvlzu-aq
    canister:alias
    ./local_import some/path/local_import.mo
    ./runtime some/path/runtime.wasm

This _reads_ only `some/path/input.mo`, and writes no files.

By transitively exploring the dependency graph using this command (and
resolving URLs appropriately before passing them as files to `moc`), one can
determine the full set of set of `.mo` files read by the two compilation modes
described above (to wasm and to IDL).

Generating documentation
------------------------

In order to generate documentation for a given Motoko package `dfx` invokes

    mo-doc
        [ --source source_dir ]
        [ --output output_dir ]
        [ --format html|adoc ]

The default source directory is `src`, the default output is `docs`, and the default format is `html`.
`mo-doc` will then generate documentation in the output directory mirroring the directory/file structure of the source directory.
