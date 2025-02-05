---
sidebar_position: 1
---

# Meet Motoko: The language of the Internet Computer

import { MarkdownChipRow } from "/src/components/Chip/MarkdownChipRow";

import Tabs from "@theme/Tabs";

import TabItem from "@theme/TabItem";

<MarkdownChipRow labels={["Beginner", "Getting started"]} />

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />

## Introduction

Motoko is the native programming language of the Internet Computer. It’s a modern, **actor-based** language designed for safety, efficiency, and scalability. With native support for asynchronous messaging, WebAssembly (Wasm) interoperability, robust type safety, and data persistence, Motoko makes it easy to build secure and performant decentralized web applications.

## Install Motoko

Follow these steps to set up your Motoko development environment:

1. **Install the IC SDK (DFX)**

  ```bash
  sh -ci "$(curl -fsSL https://internetcomputer.org/install.sh)"
  ```

  Verify the installation with:

  ```bash
  dfx --version
  ```

2. **Install Visual Studio Code**
   - Download VSCode from [https://code.visualstudio.com](https://code.visualstudio.com)
   - Follow the installation instructions for your operating system

3. **Install Motoko Extension**
   - Open VSCode
   - Go to Extensions (Ctrl+Shift+X / Cmd+Shift+X)
   - Search for "Motoko"
   - Install the official Motoko extension

## Explore Motoko use cases

<Tabs>

<TabItem value="usecase1" label="Backend" default>

Motoko makes it easy to build scalable backend services for web applications.

1. [Deploy your first Motoko canister.](https://internetcomputer.org/docs/current/tutorials/developer-journey/level-1/1.1-live-demo)

2. [Expand your application by calling external APIs from a Motoko canister.](https://internetcomputer.org/docs/current/developer-docs/backend/https-outcalls)

3. [Connect your Motoko backend with a frontend canister.](https://internetcomputer.org/docs/current/references/samples/svelte/svelte-motoko-starter/)  

</TabItem>

<TabItem value="usecase2" label="DeFi" default>

Build decentralized finance (DeFi) applications using Motoko.

1. [Set up an ICRC-1 canister.](https://internetcomputer.org/docs/current/tutorials/hackathon-prep-course/integrating-with-tokens/#deploying-an-icrc-1-ledger-locally)

2. [Create a decentralized exchange (DEX)](https://internetcomputer.org/docs/current/tutorials/developer-journey/level-5/5.3-token-swap-tutorial/)  

</TabItem>

<TabItem value="usecase3" label="Chain Fusion" default>

Develop cross-chain applications and integrate with other blockchains.  

1. [Explore Bitcoin integration (ckBTC).](https://internetcomputer.org/docs/current/developer-docs/multi-chain/bitcoin/overview)
  
2. [Explore Ethereum integration (ckETH).](https://internetcomputer.org/ethereum-integration/)
  
3. [Explore Chain-Key ERC-20 (ckERC-20) integrations.](https://internetcomputer.org/docs/current/developer-docs/multi-chain/chain-key-tokens/ckerc20/overview/)

</TabItem>

<TabItem value="usecase4" label="Digital assets" default>

Manage digital assets on the Internet Computer.

1. [Learn how to deploy NFTs on ICP](https://internetcomputer.org/docs/current/tutorials/developer-journey/level-5/5.4-NFT-tutorial)

</TabItem>

</Tabs>

## Quick References  

For developers looking for quick access to key resources, here are direct links to the base library and useful examples:

- **[Motoko base library](https://internetcomputer.org/docs/current/motoko/main/base/)** – Browse core modules and functions.  

- **[Examples:](https://internetcomputer.org/docs/current/motoko/main/getting-started/examples)**

### Share your feedback

Was this page helpful? [Yes] [No]

If you have any questions or suggestions, join the [DFINITY Developer Forum](https://forum.dfinity.org/) to share your feedback. Connect with the community, collaborate with other developers, and stay up to date with the latest Motoko updates.
