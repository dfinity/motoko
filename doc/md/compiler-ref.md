# Compiler reference

The Motoko compiler (`moc`) is the primary tool for compiling Motoko programs into executable WebAssembly (Wasm) modules. The compiler runs in the background when you build projects using the DFINITY Canister SDK. If you invoke the compiler directly on the command-line, you can press CTRL-C to exit.

This section provides compiler command-line reference information.

## moc

Use the Motoko compiler (`moc`) to compile Motoko programs into executable WebAssembly (Wasm) modules.

### Basic usage

``` bash
moc [option] [file ...]
```

### Options

You can use the following options with the `moc` command.

| Option                                    | Description                                                                                                                                           |
|-------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------|
| `--actor-idl <idl-path>`                  | Specifies a path to actor IDL (Candid) files.                                                                                                         |
| `--actor-alias <alias> <principal>`       | Specifies an actor import alias.                                                                                                                      |
| `--args <file>`                           | Read additional newline separated command line arguments from \<file>                                                                                 |
| `--args0 <file>`                          | Read additional `NUL` separated command line arguments from `<file>`                                                                                  |
| `-c`                                      | Compile to WebAssembly.                                                                                                                               |
| `--check`                                 | Performs type checking only.                                                                                                                          |
| `--compacting-gc`                         | Use compacting GC                                                                                                                                     |
| `--copying-gc`                            | Use copying GC (default)                                                                                                                              |
| `--debug`                                 | Respects debug expressions in the source (the default).                                                                                               |
| `--error-detail <n>`                      | Set level of error message detail for syntax errors, n in \[0..3\] (default 2).                                                                       |
| `-help`,`--help`                          | Displays usage information.                                                                                                                           |
| `--hide-warnings`                         | Hides compiler warnings.                                                                                                                              |
| `-Werror`                                 | Treat warnings as errors.                                                                                                                             |
| `--idl`                                   | Compile binary and emit Candid IDL specification to `.did` file.                                                                                      |
| `-i`                                      | Runs the compiler in an interactive read–eval–print loop (REPL) shell so you can evaluate program execution (implies -r).                             |
| `--map`                                   | Outputs a JavaScript source map.                                                                                                                      |
| `--max-stable-pages <n>`                  | Set maximum number of pages available for library `ExperimentStableMemory.mo` (default 65536).                                                        |
| `-no-system-api`                          | Disables system API imports.                                                                                                                          |
| `-o <file>`                               | Specifies the output file.                                                                                                                            |
| `-p <n>`                                  | Sets the print depth.                                                                                                                                 |
| `--package <package-name> <package-path>` | Specifies a `<package-name>` `<package-path>` pair, separated by a space.                                                                             |
| `--public-metadata <name>`                | Emit icp custom section `<name>` (`candid:args` or `candid:service` or `motoko:stable-types` or `motoko:compiler`) as `public` (default is `private`) |
| `--omit-metadata <name>`                  | omit icp custom section `<name>` (`candid:args` or `candid:service` or `motoko:stable-types` or `motoko:compiler`)                                    |
| `--print-deps`                            | Prints the dependencies for a given source file.                                                                                                      |
| `-r`                                      | Interprets programs.                                                                                                                                  |
| `--release`                               | Ignores debug expressions in the source.                                                                                                              |
| `--stable-types`                          | Compile binary and emit signature of stable types to `.most` file.                                                                                    |
| `--stable-compatible <pre> <post>`        | Test upgrade compatibility between stable-type signatures `<pre>` and `<post>`.                                                                       |
| `-t`                                      | Activates tracing in interpreter.                                                                                                                     |
| `-v`                                      | Generates verbose output.                                                                                                                             |
| `--version`                               | Displays version information.                                                                                                                         |
| `-wasi-system-api`                        | Uses the WASI system API (`wasmtime`).                                                                                                                |
