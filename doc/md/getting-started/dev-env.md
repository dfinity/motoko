---
sidebar_position: 3
---


# Developer environment

## Overview

To develop and deploy Motoko canister smart contracts, you will need a developer environment that contains the Motoko compiler and base library. It is recommended to use the IC SDK, which includes Motoko, along with `dfx`, a command-line tool used to create, build, and deploy canisters on ICP.

Developer environments come in several types and formats, making developing flexible and accessible.

## Cloud environments

Cloud developer environments, such as Gitpod or GitHub Codespaces, are browser-based developer environments that can be configured with the necessary tools to develop Motoko code.

Learn more about [Gitpod](/docs/current/developer-docs/developer-tools/ide/gitpod) and [GitHub Codespaces](/docs/current/developer-docs/developer-tools/ide/codespaces) for Motoko development.

## Container environments

Developers may want to setup a containerized environment for Motoko and other ICP-related development. Container environments are especially useful for Windows-based systems, since `dfx` is not natively supported on Windows.

Learn more about [developer containers](/docs/current/developer-docs/developer-tools/ide/dev-containers) and [Docker containers](/docs/current/developer-docs/developer-tools/ide/dev-containers#using-docker-directly) for Motoko development.

## Motoko playground

Motoko playground is a browser-based developer environment that allows for temporary deployment and testing of canister smart contracts. Motoko playground can also be run locally, or it can be utilized through the `dfx deploy --playground` command via the CLI.

Canisters deployed to the Motoko playground use borrowed resources from a canister pool and are limited to a deployment length of 20 minutes. Therefore, the playground is not recommended for long-term development.

Learn more about the [Motoko playground](/docs/current/developer-docs/developer-tools/ide/playground).

## Local developer environment

Before you start developing Motoko, verify the following:

- [x] You have an internet connection and access to a shell terminal on your local macOS or Linux computer.

- [x] You have a command line interface (CLI) window open. This window is also referred to as the 'terminal' window.

- [x] You have downloaded and installed the IC SDK package as described in the [installing the IC SDK](/docs/current/developer-docs/getting-started/install/) page.

- [x] You have a code editor installed. The [VSCode IDE](https://code.visualstudio.com/download) is a popular choice.

- [x] You have downloaded and installed [git](https://git-scm.com/downloads).

- [x] Assure that all packages and tools above are updated to the latest release versions.
