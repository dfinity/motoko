# Motoko + Rust Starter

[![Open in Gitpod](https://gitpod.io/button/open-in-gitpod.svg)](https://gitpod.io/#https://github.com/rvanasa/motoko-rust-starter)

> Note: running in-browser currently takes a while due to building the Motoko compiler from source.

## 📦 Overview

This is an example project for calling Rust functions from a Motoko program using the [WebAssembly Component Model](https://component-model.bytecodealliance.org/). While this is currently intended to be used with [Wasmtime](https://github.com/bytecodealliance/wasmtime#readme), it's possible to run the generated Motoko + Rust component in any environment with component model support. 

## ⚙️ Getting Started

### System Requirements

* Unix operating system (tested on Ubuntu and macOS)
* [Rust](https://www.rust-lang.org/)
* [Wasmtime](https://github.com/bytecodealliance/wasmtime#readme)
* [`wasm-tools`](https://github.com/bytecodealliance/wasm-tools#readme)
* [`wac`](https://github.com/bytecodealliance/wac#readme)

### Motoko Compiler and Libraries

Functionality from components can be imported using the usual `import`-syntax, with a special `component:`-prefix (see [Main.mo](./src/motoko/Main.mo) for an example).  The reason 
for the special prefix is to make it explicit, that the resulting Wasm is in a Wasm-component-binary (rather than Wasm-module-binary).  To enable compilation with components a flag `-wasm-components` has to be passed to `moc`. 

### `lib`-folder 

The project includes scripts in this folder for building and running a Motoko example program with Wasm components, plus some auxiliary scripts and data in  `/lib`-folder: 
- [`set_env.sh`](./lib/set_env.sh) contains common settings used by the individual building steps.
- `wasi-adapter.wasm` downloaded from [here](https://github.com/bytecodealliance/wasmtime/releases/download/v22.0.1/wasi_snapshot_preview1.command.wasm), used when creating
  Motoko component binary.
- [`lib/mops/`](./lib/mops/) contains pre-build example Rust components to be used
  in Motoko.  The `component/` subfolder uses a folder structure that mimicks the layout 
  used by `mops`.
- [`build_rust.sh`](./lib/build_rust.sh) can be used to re-build the Rust components
  (from sources in [`src/rust/`](./src/rust/)), if needed.  The script updates 
  the components "distributed" in [`lib/mops/`](./lib/mops/).

**Current main limitations/known issues, roughly in the order of priorities**

- compiling a Motoko source that uses components requires setting `MOC_UNLOCK_PRIM` env variable.
- supports primitive types, arrays and variants as arguments or return values, 
  but no records/structs yet
- the expected layout of the distributed components (cf. [lib/mops/component/](./lib/mops/component/)) is somewhat weird (and should be changed), due to various conventions wrt. underscore- and hyphen-characters,
  see comments in [build_rust.sh](./lib/build_rust.sh) for more info
- the generated Motoko binaries use hard-coded motoko-string in their names; 
  instead the binaries's names should probably depend on the name of the Motoko sources.
