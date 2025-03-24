# Why develop applications to run on the internet computer?

Motoko provides:

-   A high-level language for programming applications to run on the Internet Computer blockchain network.

-   A simple design that uses familiar syntax that is easy for programmers to learn.

-   An **actor-based** programming model optimized for efficient message handling.

-   An interpreter and compiler that you can use to test and compile the WebAssembly code for autonomous applications.

-   Support for features not-yet implemented that anticipate future extensions and improvement to WebAssembly.

# Why a new language?

The Internet Computer provides a blockchain network that can support programs written in different languages. The only requirement is that the program must support compilation to WebAssembly code. WebAssembly (commonly-abbreviated as Wasm) is a low-level computer instruction format for virtual machines. Because WebAssembly code is designed to provide portable low-level instructions that enable applications to be deployed on many platforms such as the web, it is a natural fit for deploying applications that are intended to run on the Internet Computer. However, most of the higher-level languages - like C, C, and Rust - that support compiling to WebAssembly are either too unsafe (for example, C or C) or too complex (for example, Rust) for developers who want to deliver secure applications without a long learning curve.

To address the need for correctness without complexity, DFINITY has designed its own **Motoko** programming language. **Motoko** provides a simple and expressive alternative to other programming languages that is easy to learn whether you are a new or experienced programmer.

# Support for other languages

WebAssembly is language-agnostic. It does not require a high-level type system for language inter-operation. Although Motoko is specifically designed to compile to WebAssembly and make it easy to write programs to run on the internet computer, it is just one of many languages you can eventually use to develop applications for the Internet Computer blockchain network.

To support multiple languages and typed, cross-language communication, DFINITY also provides an **Interface Definition Language** (IDL). The Motoko compiler automates the production and consumption of IDL files using the type signatures in Motoko programs and the structure of imported IDL interfaces.

For information about the **Interface Definition Language** interfaces, see XXX.
