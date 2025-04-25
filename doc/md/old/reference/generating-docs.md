---
sidebar_position: 4
---

# Generating Motoko documentation



`mo-doc` is a command-line tool for generating documentation for Motoko source code. It processes source files and generates documentation in various formats.

## Quickstart

Download `mo-doc` from Motoko's [GitHub releases page](https://github.com/dfinity/motoko/releases) or simply use the binary included in your [dfx](https://internetcomputer.org/docs/current/developer-docs/setup/install) installation:

```
$(dfx cache show)/mo-doc [options]
```

## Options

- `--source <path>`: Specifies the directory to search for Motoko source files. Defaults to `src`.

- `--output <path>`: Specifies the directory where the documentation will be generated. Defaults to `docs`.

- `--format <format>`: Specifies the generated format. Should be one of the following:
  - `html`: Generates HTML format documentation.
  - `adoc`: Generates AsciiDoc format documentation.
  - `plain`: Generates Markdown documentation.

  Defaults to `html`.

- `--help`: Shows usage information.

## Examples

1. Generate HTML documentation from the default source directory (`src`) and place it in the default output directory (`docs`):

   ```bash
   mo-doc
   ```

2. Generate AsciiDoc documentation from a specific source directory:

   ```bash
   mo-doc --format plain --source ./motoko-code
   ```

3. Generate Markdown documentation in a custom output directory:

   ```bash
   mo-doc --format adoc --output ./public
   ```

## Writing doc comments

`mo-doc` supports documenting your Motoko code using special block comments (`/** */`) and line comments (`///`).

Doc comments can be used to provide explanations for functions, classes, types, modules, variables, and more. They can span multiple lines and may contain rich Markdown formatting:

```motoko no-repl
/// Calculate the factorial of a given positive integer.
///
/// Example:
/// ```motoko
/// factorial(0); // => null
/// factorial(3); // => ?6
/// ```
func factorial(n : Nat) : ?Nat {
    // ...
}
```

## Resources
Check out Motoko's [base library source code](https://github.com/dfinity/motoko-base/tree/master/src) for additional examples and best practices.

The source code for `mo-doc` is available in the [dfinity/motoko](https://github.com/dfinity/motoko/tree/master/src/docs) GitHub repository. Contributions are welcome!

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />