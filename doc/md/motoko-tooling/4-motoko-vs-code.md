---
sidebar: 4
---

# Motoko Visual Studio Code extension

Visual Studio (VS) Code is a [widely used](https://survey.stackoverflow.co/2022/#section-worked-with-vs-want-to-work-with-integrated-development-environment) open-source IDE that supports canister development in [Motoko](https://internetcomputer.org/docs/motoko/getting-started/motoko-introduction) and [Rust](https://www.rust-lang.org/).

## Motoko

The [Motoko VS Code extension](https://github.com/dfinity/vscode-motoko) provides type checking, formatting, autocompletion, go-to-definition, code snippets, and more for Motoko canister development.

[![Showcase](https://github.com/dfinity/vscode-motoko/raw/master/guide/assets/intro.webp)](https://marketplace.visualstudio.com/items?itemName=dfinity-foundation.vscode-motoko)

### Installation

[![Visual Studio Marketplace](https://img.shields.io/visual-studio-marketplace/v/dfinity-foundation.vscode-motoko?color=brightgreen&logo=visual-studio-code)](https://marketplace.visualstudio.com/items?itemName=dfinity-foundation.vscode-motoko)

Install the extension through the [VS Marketplace](https://marketplace.visualstudio.com/items?itemName=dfinity-foundation.vscode-motoko), or alternatively the [**Extensions** panel](https://code.visualstudio.com/docs/editor/extension-marketplace) in your VS Code project.

[VSCodium](https://vscodium.com/) users can download the extension from [Open VSX](https://open-vsx.org/extension/dfinity-foundation/vscode-motoko) or the [GitHub releases](https://github.com/dfinity/vscode-motoko/releases) page.

### Keyboard shortcuts

Below are the default key bindings for commonly used features supported in the extension:

- **Code formatter** (`Shift` + `Alt` + `F`): format a Motoko file using [prettier-plugin-motoko](https://github.com/dfinity/prettier-plugin-motoko).
- **Organize imports** (`Shift` + `Alt` + `O`): group and sort imports at the top of your Motoko file.
- **Import code action** (`Ctrl/Cmd` + `.` while hovering over an unresolved variable): show import quick-fix options.
- **Go to definition** (`F12`): jump to the definition of a local or imported identifier.
- **IntelliSense** (`Ctrl` + `Space`): view all available auto-completions and code snippets.

[![Snippets](https://github.com/dfinity/vscode-motoko/raw/master/guide/assets/snippets.png)](https://marketplace.visualstudio.com/items?itemName=dfinity-foundation.vscode-motoko)

### Other features

- [Vessel](https://github.com/dfinity/vessel) and [Mops](https://mops.one/) (the two most popular Motoko package managers) are supported out-of-the-box in this extension.
- Quickly convert between Motoko types using code snippets such as `array-2-buffer` or `principal-2-text`.
- In case you're hoping to learn Motoko without installing `dfx`, the Motoko VS Code extension works standalone on all major operating systems (including Windows).
- This extension also provides schema validation and autocompletion for `dfx.json` config files.
- View type information and documentation by hovering over function names, imports, and other expressions.

[![Tooltips](https://github.com/dfinity/vscode-motoko/raw/master/guide/assets/tooltips.png)](https://marketplace.visualstudio.com/items?itemName=dfinity-foundation.vscode-motoko)

#### Contributing

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg?logo=github)](https://github.com/dfinity/prettier-plugin-motoko)

The Motoko VS Code extension is completely open-source and [available on GitHub](https://github.com/dfinity/vscode-motoko).

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />