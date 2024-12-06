---
sidebar_position: 3
---

# Motoko dev server



Motoko dev server, or `mo-dev` for short, is a command line tool that features a development server with live reloading for Motoko.

## Installation

### Prerequisites

- [Node.js](https://nodejs.org/en/) ≥ 16:

You can install `mo-dev` with `npm`:

```sh
npm i -g mo-dev
```

:::info

Standalone `mo-dev` binaries are also available as [GitHub releases](https://github.com/dfinity/motoko-dev-server/releases).

:::

## Usage

Specify the working directory of your Motoko project, which must contain a `dfx.json` file:

```sh
mo-dev --cwd path/to/dfx_project
```

Then, run the dev server for a specific canister:

```sh
mo-dev --canister foo --deploy
```

You can deploy all canisters within a `dfx.json` file with `--deploy` flag. Canisters will be deployed when a Motoko file is changed:

```sh
mo-dev --deploy
```

You can also pass an installation argument to `dfx deploy`:

```sh
mo-dev --deploy --argument '()'
```

[View the full usage of `mo-dev`](https://github.com/dfinity/motoko-dev-server/).

### Testing

`mo-dev` supports running unit tests (`*.test.mo`). Tests will be run when a Motoko file is changed:

```sh
mo-dev --test
```

`mo-dev` also includes a `mo-test` command, which can be used to run unit tests within CI workflows.

To run all Motoko unit tests (`*.test.mo`), use the command:

```sh
mo-test
```

You can also run all Motoko unit tests using a WASI runtime by default. This has a higher performance, requires installing [Wasmtime](https://wasmtime.dev/) on your system:

```sh
mo-test --testmode wasi
```

To configure the runtime of an individual unit test, include the following comment within your test file (`*.test.mo`):

```motoko no-repl
// @testmode wasi
```

[View the full usage of `mo-test`](https://github.com/dfinity/motoko-dev-server/?tab=readme-ov-file#mo-test).


## Examples

The [Vite + React + Motoko](https://github.com/rvanasa/vite-react-motoko#readme) project showcases how to integrate `mo-dev` into a full-stack dapp.

[![Open in Gitpod](https://gitpod.io/button/open-in-gitpod.svg)](https://gitpod.io/#https://github.com/rvanasa/vite-react-motoko)

## Important notes

`mo-dev` is early in development. Please feel free to report a bug, ask a question, or request a feature on the project's [GitHub issues](https://github.com/dfinity/motoko-dev-server/issues) page.


<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />
