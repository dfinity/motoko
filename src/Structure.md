Project file structure
======================

Since we switched to `dune`, we are organizing the Ocaml source in libraries
and executables.

Every library is a subdirectory of `src/`, contains a `dune` file that
indicates its name and other library dependencies. All `.ml` files in that
directory are part of the library.

Within a library `lib`, modules are accessed using `Foo`; outside via
`Lib.Foo`. Unless there is `lib/libml`, then this is the entry point.


Libraries
---------

We split the files into the following subdirectories, topologically sorted. One
goal of the structure is to make it clear which parts operate on AS Source, AS
IR or Wasm, respectively.

In parenthesis: which end-product is using these file, and which libraries this
is should depend on, omitting transitive dependencies. See the `*/dune` files
for the real deal; some shortcuts had to be taken.

 * `lib/` (all)

   Stuff that could be in the Ocaml standard library.

 * `lang_utils/` (asc, didc; using `lib/`)

   General PL-related utility-functions, useful for AS Source, AS IR, the IDL
   AST: Environments, diagnostic error messages, source locations.

 * `as_def/` (asc; using `lang_utils/`)

   The ActorScript AST and pretty-printer.

 * `as_types/` (asc; using `lang_utils/`)

   The ActorScript type definition, as used by both Source and IR. Includes
   pretty-printer.

 * `as_values/` (asc; using `lang_utils/`)

   Value definitions, as used for literals, interpreter. Also includes
   operations on values and primitive operations.

 * `as_frontend/` (asc; using `lang_utils/` and `as_def`)

   The ActorScript parser and type checker.

 * `ir_def/` (asc; using `lang_utils/`)

   The ActorScript IR AST, including type checker and pretty-printer.

 * `lowering/` (asc; using `as_def/` and `ir_def/`)

   The IR to Source pass.

 * `ir_passes/` (asc; using `as_def/`)

   The various IR to IR passes.

 * `wasm_exts/` (asc, as-ld; using `lib/`)

   Extensions to the wasm library: Support for additional custom sections,
   including serialization and de-serialization.

 * `linking/` (asc, as-ld; using `wasm_exts/`)

   Wasm linking code

 * `codegen/` (asc; using `ir_def/`, `linking/`)

   The backend, including the instruction list generator.

 * `interpreter/` (asc; using `as_def/`)

   Source interpreter.

 * `ir_interpreter/` (asc; using `ir_def/`)

   IR interpreter.

 * `pipeline/` (asc; using `as_frontend/`, `lowering/`, `ir_passes/`, `codegen/`, `as_interpreter/`, `ir_interpreter/`)

   The pipeline, prelude text and flags

 * `idllib/`

   Kitchen-sink of `didc` related files. Yet to be split up.

 * `lsp/`

   Language Server Protocol JSON mapping.

 * `languageServer/`

   The language server functionality itself.

Executables
-----------

All exectuables are in the directory `exe/`, and should be kept rather small;
essentially only the command line parsing should be there, so that
actual functionality is easily shared.

 * `asc` (using `pipeline/`)

   The ActorScript compiler

 * `as-ide` (using `languageServer/`)

   The ActorScript language server

 * `as.js` (using `pipeline/`)

   The ActorScript compiler, as a JS library

 * `as-ld` (using `linking/`)

   The stand-alone Wasm linker

 * `didc` (using `idl/`)

   The IDL compiler
