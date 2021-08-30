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

 * `lang_utils/` (moc; using `lib/`)

   General PL-related utility-functions, useful for AS Source, AS IR, the IDL
   AST: Environments, diagnostic error messages, source locations.

 * `mo_def/` (moc; using `lang_utils/`)

   The Motoko AST and pretty-printer.

 * `mo_types/` (moc; using `lang_utils/`)

   The Motoko type definition, as used by both Source and IR. Includes
   pretty-printer.

 * `mo_values/` (moc; using `lang_utils/`)

   Value definitions, as used for literals, interpreter. Also includes
   operations on values and primitive operations.

 * `mo_frontend/` (moc; using `lang_utils/` and `mo_def`)

   The Motoko parser and type checker.

 * `ir_def/` (moc; using `lang_utils/`)

   The Motoko IR AST, including type checker and pretty-printer.

 * `lowering/` (moc; using `mo_def/` and `ir_def/`)

   The IR to Source pass.

 * `ir_passes/` (moc; using `mo_def/`)

   The various IR to IR passes.

 * `wasm_exts/` (moc, mo-ld; using `lib/`)

   Extensions to the wasm library: Support for additional custom sections,
   including serialization and de-serialization.

 * `linking/` (moc, mo-ld; using `wasm_exts/`)

   Wasm linking code

 * `codegen/` (moc; using `ir_def/`, `linking/`)

   The backend, including the instruction list generator.

 * `interpreter/` (moc; using `mo_def/`)

   Source interpreter.

 * `ir_interpreter/` (moc; using `ir_def/`)

   IR interpreter.

 * `pipeline/` (moc; using `mo_frontend/`, `lowering/`, `ir_passes/`, `codegen/`, `mo_interpreter/`, `ir_interpreter/`)

   The pipeline, prelude text and flags

 * `idllib/`

   Kitchen-sink of Candid related files. Yet to be split up.

 * `lsp/`

   Language Server Protocol JSON mapping.

 * `languageServer/`

   The language server functionality itself.

Executables
-----------

All exectuables are in the directory `exe/`, and should be kept rather small;
essentially only the command line parsing should be there, so that
actual functionality is easily shared.

 * `moc` (using `pipeline/`)

   The Motoko compiler

 * `mo-ide` (using `languageServer/`)

   The Motoko language server

 * `mo.js` (using `pipeline/`)

   The Motoko compiler, as a JS library

 * `mo-ld` (using `linking/`)

   The stand-alone Wasm linker
