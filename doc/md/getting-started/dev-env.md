---
sidebar_position: 3
---


# Developer environment

## Overview

To develop and deploy Motoko canister smart contracts, you will need a developer environment that contains the Motoko compiler and base library. It is recommended to use the [IC SDK](https://github.com/dfinity/sdk#readme), which includes Motoko, along with `dfx`, a command-line tool used to create, build, and deploy canisters on ICP.

Developer environments come in several types and formats, making developing flexible and accessible.

## Cloud environments

[Gitpod](https://www.gitpod.io/) and [GitHub Codespaces](https://github.com/features/codespaces) are browser-based development environments that can be used to build, test, and run Motoko smart contracts.

Here are some starter projects for online Motoko canister development:

* [ICP Hello World Motoko](https://github.com/dfinity/icp-hello-world-motoko#readme)
* [Vite + React + Motoko](https://github.com/rvanasa/vite-react-motoko#readme)

Learn more about [Gitpod](/docs/current/developer-docs/developer-tools/ide/gitpod) and [GitHub Codespaces](/docs/current/developer-docs/developer-tools/ide/codespaces) for Motoko development.

## Container environments

Developers may want to setup a containerized environment for Motoko and other ICP-related development. Container environments are especially useful for Windows-based systems, since `dfx` is not natively supported on Windows.

Learn more about [developer containers](/docs/current/developer-docs/developer-tools/ide/dev-containers) and [Docker containers](/docs/current/developer-docs/developer-tools/ide/dev-containers#using-docker-directly) for Motoko development.

## Motoko playground

[Motoko playground](https://play.motoko.org/) is a browser-based developer environment that allows for temporary deployment and testing of canister smart contracts. Motoko playground can also be utilized through the `dfx deploy --playground` command via the CLI.

Canisters deployed to the Motoko playground use borrowed resources from a canister pool and are limited to a deployment length of 20 minutes. Therefore, the playground is not recommended for long-term development.

Learn more about the [Motoko playground](/docs/current/developer-docs/developer-tools/ide/playground).

## Local developer environment

Before you start developing Motoko, verify the following:

- [x] You have an internet connection and access to a shell terminal on your local macOS or Linux computer.

- [x] You have a command line interface (CLI) window open. This window is also referred to as the 'terminal' window.

- [x] You have downloaded and installed the IC SDK package as described in the [installing the IC SDK](/docs/current/developer-docs/getting-started/install/) page.

- [x] You have a code editor installed. The [VS Code IDE](https://code.visualstudio.com/download) (with the [Motoko extension](https://marketplace.visualstudio.com/items?itemName=dfinity-foundation.vscode-motoko)) is a popular choice.

- [x] You have downloaded and installed [git](https://git-scm.com/downloads).

- [x] Assure that all packages and tools above are updated to the latest release versions.

## Motoko version

Motoko version `0.11.1` is shipped with the latest version of the IC SDK (v0.20.0).

### Specifying a custom version of the compiler

To specify a custom version of the Motoko compiler to be used with `dfx`, you can use the package manager `mops` or `vessel`.

For `mops`, use the following command to download a different version of the Motoko compiler (`moc`):

```
mops toolchain use moc 0.10.3
```

For `vessel`, set the following environment variable:

```
DFX_MOC_PATH="$(vessel bin)/moc" dfx deploy
```

## Specifying a custom version of the base library

To specify a custom version of the Motoko base library to be used with `dfx`, you can use the package manager `mops` with the following command:

```
mops add base@<VERSION> && mops install
```

For example, to use base library version `0.9.0`, use the command:

```
mops add base@0.9.0 && mops install
```