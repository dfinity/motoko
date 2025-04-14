---
sidebar: 1
---

# Canpack


Canpack is a code generation tool designed to simplify communication across [canisters](https://internetcomputer.org/docs/building-apps/essentials/canisters) written in different languages. It currently supports calling a Rust crate from Motoko code. Canpack generates a separate canister for the host language, then combines the other language's code fragments that are defined across different libraries.

Canpack supports the [Mops](https://mops.one/) package manager.

:::caution
Canpack is still in early development. Breaking changes may be introduced at any time.
:::

## Installation

<Tabs>
<TabItem value="prereq" label="Prerequisites" default>

<input type="checkbox"/> <a href="/docs/building-apps/getting-started/install">Install the IC SDK.</a> Note: While using the IC SDK is the typical path for most developers, experienced Rust developers may choose to circumvent IC SDK entirely and use the <a href="https://github.com/dfinity/cdk-rs"> Rust CDK </a> directly.
<div>
</div>
<input type="checkbox"/> <a href="https://doc.rust-lang.org/book/ch01-01-installation.html">Download and install Rust.</a>
<div>
</div>
<input type="checkbox"/> Download and install the <code>wasm32-unknown-unknown</code> target: <code>rustup target add wasm32-unknown-unknown</code>
</TabItem>
</Tabs>

Then, install the Canpack CLI using `npm`:

```
npm install -g canpack
```

## Using Canpack

In a project directory that contains a `dfx.json` and `mops.toml` file, you can use Canpack by first creating a new file called `canpack.json` with the following content:

```json
{
    "canisters": {
        "motoko_rust": {
            "type": "rust",
            "parts": [{
                "package": "canpack-example-hello",
                "version": "^0.1"
            }]
        }
    }
}
```

Then, generate the required files using the command:

```bash
canpack
```

In the project's `dfx.json` file, configure the `"dependencies"` for your project's Motoko canister:

```json
{
    "canisters": {
        "my_project_backend": {
            "dependencies": ["motoko_rust"],
            "main": "src/my_project_backend/main.mo",
            "type": "motoko"
        }
    },
}
```

Then, to write a Motoko canister that imports Rust crates, import `Rust` from the `motoko_rust` canister:

```motoko no-repl
import Rust "canister:motoko_rust";

actor {
    public composite query func hello(name: Text) : async Text {
        await Rust.canpack_example_hello(name)
    }
}
```

Motoko [canisters](https://internetcomputer.org/docs/building-apps/essentials/canisters) can import any Rust crate that is compatible with Canpack. Canpack supports any IC Wasm-compatible crate.

### Adding Canpack support to a crate

To add Canpack support to a Rust crate, export a Rust function with `canpack::export!`:

```rust
canpack::export! {
    pub fn canpack_example_hello(name: String) -> String {
        format!("Hello, {name}!")
    }
}
```

`canpack::export!` requires that you add [`canpack`](https://crates.io/crates/canpack) as a dependency in your `Cargo.toml` file.

### Reference local data

You can also reference local data, such as methods and constants:

```rust
const WELCOME: &str = "Welcome";

fn hello(salutation: &str, name: String) -> String {
    format!("{salutation}, {name}!")
}

canpack::export! {
    pub fn canpack_example_hello(name: String) -> String {
        hello(WELCOME, name)
    }
}
```

### Candid methods

To configure an automatically generated Candid method for a function, use the `#[canpack]` attribute,

```rust
canpack::export! {
    #[canpack(composite_query, rename = "canpack_example_hello")]
    pub fn hello(name: String) -> String {
        format!("Hello, {name}!")
    }
}
```

Alternatively, you can manually define a Candid method through the `canpack!` macro:

```rust
pub fn hello(name: String) -> String {
    format!("Hello, {name}!")
}

#[macro_export]
macro_rules! canpack {
    () => {
        #[ic_cdk::query]
        #[candid::candid_method(query)]
        fn canpack_example_hello(name: String) -> String {
            $crate::hello(name)
        }
    };
}
```

## Resources

Canpack is open to [contributions](https://github.com/dfinity/canpack/blob/main/.github/CONTRIBUTING.md) and encourages you to report bugs, ask questions, or request features using the project's [GitHub issues](https://github.com/dfinity/canpack/issues).

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />