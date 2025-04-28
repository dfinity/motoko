---
sidebar_position: 2
---

# Motoko formatting tools



The Motoko Prettier plugin can be used to format and validate Motoko source code files. It can be used through the [Prettier CLI](https://prettier.io/docs/en/cli.html) or through VS Code.

Alternatively, the Node.js package [`mo-fmt`](https://www.npmjs.com/package/mo-fmt) is a standalone Motoko formatter tool.

## Motoko Prettier plugin

### Installation

To install the Motoko Prettier plugin, first download and install [Node.js](https://nodejs.org/en/download/).

Then, create a new Motoko project or navigate into an existing project directory. [Learn more about creating a Motoko project](../getting-started/quickstart.md).

Then, run the following command in the project directory:

```sh
npm install --save-dev prettier prettier-plugin-motoko
```

### Using the Prettier CLI

You can format Motoko files through the Prettier CLI using the command:

```sh
npx prettier --write --plugin=prettier-plugin-motoko **/*.mo
```

To validate if your Motoko files are formatted properly, use the following command:

```sh
npx prettier --check --plugin=prettier-plugin-motoko **/*.mo
```

### Using VS Code

The Motoko Prettier plugin works out of the box with the [Motoko extension for VS Code](https://marketplace.visualstudio.com/items?itemName=dfinity-foundation.vscode-motoko).

It is also compatible with the the [Prettier extension](https://marketplace.visualstudio.com/items?itemName=esbenp.prettier-vscode).

### Ignoring code

You can set code to be skipped from formatting or being validated by using a `prettier-ignore` comment:

```motoko no-repl
// prettier-ignore
func ignored<A>(a:A){a};

func formatted<B>(b : B) { b };
```

## `mo-fmt`

### Installation

To install `mo-fmt`, run the following command:

```
npm install mo-fmt
```

Then, format and validate the format of Motoko code files, run the commands:

```
mo-fmt **/*
mo-fmt -c **/*
```

## References

- [Motoko Prettier plugin GitHub repo](https://github.com/dfinity/prettier-plugin-motoko/)

- [Motoko extension for VS Code](https://marketplace.visualstudio.com/items?itemName=dfinity-foundation.vscode-motoko)

- [`mo-fmt`](https://www.npmjs.com/package/mo-fmt)

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />
