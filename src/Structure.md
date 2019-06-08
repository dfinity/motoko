Project file structure
======================

Since we switched to `dune`, we are organizing the Ocaml source in libraries
and executables.

Every library is a subdirectory of `src/`, contains a `dune` file that
indicates its name and other library dependencies. All `.ml` files in that
directory are part of the library.

Within a library `lib`, modules are accessed using `Foo`; outside via
`Lib.Foo`. Unless there is `lib/libml`, then this is the entry point.

Executables
---------

All exectuables are in the directory `exe/`, and should be kept rather small;
essentially only the command line parsing should be there, so that
actual functionality is easily shared.


Libraries
---------

We split the files into the following subdirectories

 * `lib/`

   Stuff that could be in the Ocaml standard library.

 * `aslib/`

   Kitchen-sink of `asc` and `as-ld` related files. Will be split soon.

 * `idllib/`

   Kitchen-sink of `didc` related files. Will likely be split soon.

 * `lsp/`

   Language Server Protocol JSON mapping.

 * `languageServer/`

   The language server functionality itself.

