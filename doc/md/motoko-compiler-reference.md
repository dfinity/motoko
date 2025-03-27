---
sidebar_position: 8
---

# Compiler reference

The Motoko compiler (`moc`) is used to compile Motoko programs into executable WebAssembly (Wasm) modules. The compiler runs in the background when you build projects using the [IC SDK](https://internetcomputer.org/docs/current/developer-docs/setup/install).

Alternatively, the compiler can be run directly on the command-line.

This section provides compiler command-line reference information.

## mocv

`mocv` is a version manager for the Motoko compiler (`moc`) used to easily switch between different versions of the compiler without relying on `dfx`. This tool is particularly useful for projects that need specific compiler versions or for developers who want to experiment with different Motoko features. [Learn more about mocv](https://github.com/ZenVoich/mocv).

### Installing mocv

```bash
npm i mocv -g
```

### Initialize mocv environment

```bash
mocv init
```

This command:

1. Configures your environment to use the selected `moc` version.
2. Adds a line to your `.bashrc` (or equivalent) to set `DFX_MOC_PATH` to the current `moc` version.
3. Asks you to restart your terminal or source your profile.

Once you've set up `mocv`, you can use the Motoko compiler directly. Here's the basic usage:

## `moc` basic usage

``` bash
moc [option] [file ...]
```

### Options

You can use the following options with the `moc` command:

| Option                                    | Description                                                                                                                                           |
|-------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------|
| `--ai-errors`                             | Emit AI tailored error messages.                                                                                                                      |
| `--actor-idl <idl-path>`                  | Specifies a path to actor IDL (Candid) files.                                                                                                         |
| `--actor-alias <alias> <principal>`       | Specifies an actor import alias.                                                                                                                      |
| `--args <file>`                           | Read additional newline separated command line arguments from `<file>`.                                                                               |
| `--args0 <file>`                          | Read additional `NUL` separated command line arguments from `<file>`.                                                                                 |
| `-c`                                      | Compile to WebAssembly.                                                                                                                               |
| `--check`                                 | Perform type checking only.                                                                                                                          |
| `--compacting-gc`                         | Use compacting GC (not supported with enhanced orthogonal persistence).                                                                               |
| `--copying-gc`                            | Use copying GC (default with classical persistence, not supported with enhanced orthogonal persistence).                                              |
| `--debug`                                 | Respect debug expressions in the source (the default).                                                                                               |
| `--enhanced-orthogonal-persistence`       | Use enhanced orthogonal persistence (experimental): Scalable and fast upgrades using a persistent 64-bit main memory.                                 |
| `--error-detail <n>`                      | Set level of error message detail for syntax errors, n in \[0..3\] (default 2).                                                                       |
| `--experimental-stable-memory <n>`        | Select support for the deprecated `ExperimentalStableMemory.mo` library (n < 0: error, n = 0: warn, n > 0: allow) (default 0).                        |
| `-fno-shared-code`                        | Do not share low-level utility code. Larger code size but decreased cycle consumption (default).                                                      |
| `--generational-gc`                       | Use generational GC (not supported with enhanced orthogonal persistence).                                                                             |
| `-fshared-code`                           | Do share low-level utility code. Smaller code size but increased cycle consumption.                                                                   |
| `-help`,`--help`                          | Display usage information.                                                                                                                           |
| `--hide-warnings`                         | Hide compiler warnings.                                                                                                                              |
| `-Werror`                                 | Treat warnings as errors.                                                                                                                             |
| `--incremental-gc`                        | Use incremental GC (default of enhanced orthogonal persistence, also available for classical persistence).                                            |
| `--idl`                                   | Compile binary and emit Candid IDL specification to `.did` file.                                                                                      |
| `-i`                                      | Run the compiler in an interactive read–eval–print loop (REPL) shell so you can evaluate program execution (implies -r).                             |
| `--map`                                   | Output a JavaScript source map.                                                                                                                      |
| `--max-stable-pages <n>`                  | Set a maximum number of pages available for library `ExperimentStableMemory.mo` (default 65536).                                                        |
| `-no-system-api`                          | Disable system API imports.                                                                                                                          |
| `-no-timer`                               | Disable timer API imports and hide timer primitives.                                                                                                |
| `-o <file>`                               | Specify the output file.                                                                                                                            |
| `-p <n>`                                  | Set the print depth.                                                                                                                                 |
| `--package <package-name> <package-path>` | Specify a `<package-name>` `<package-path>` pair, separated by a space.                                                                             |
| `--public-metadata <name>`                | Emit ICP custom section `<name>` (`candid:args` or `candid:service` or `motoko:stable-types` or `motoko:compiler`) as `public` (default is `private`).|
| `--omit-metadata <name>`                  | Omit ICP custom section `<name>` (`candid:args` or `candid:service` or `motoko:stable-types` or `motoko:compiler`).                                   |
| `--print-deps`                            | Print the dependencies for a given source file.                                                                                                      |
| `-r`                                      | Interpret programs.                                                                                                                                  |
| `--release`                               | Ignore debug expressions in the source.                                                                                                              |
| `--stable-regions`                        | Force eager initialization of stable regions metadata (for testing purposes); consumes between 386KiB or 8MiB of additional physical stable memory, depending on current use of `ExperimentalStableMemory`. |
| `--stable-types`                          | Compile binary and emit signature of stable types to `.most` file.                                                                                    |
| `--stable-compatible <pre> <post>`        | Test upgrade compatibility between stable-type signatures `<pre>` and `<post>`.                                                                       |
| `--rts-stack-pages <n>`                   | Set maximum number of pages available for runtime system stack (only supported with classical persistence, default 32).                               |
| `--trap-on-call-error`                    | Trap, don't throw an [`Error`](../base/Error.md), when an IC call fails due to the destination queue being full or crossing a freezing threshold. Emulates behaviour of moc versions < 0.8.0.               |
| `-t`                                      | Activate tracing in interpreter.                                                                                                                     |
| `-v`                                      | Generate verbose output.                                                                                                                             |
| `--version`                               | Display version information.                                                                                                                         |
| `-wasi-system-api`                        | Use the WASI system API (`wasmtime`).                                                     |
