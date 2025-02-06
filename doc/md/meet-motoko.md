---
sidebar_position: 1
---

# Meet Motoko: The language shaping the future of Web3

import { MarkdownChipRow } from "/src/components/Chip/MarkdownChipRow";

import Tabs from "@theme/Tabs";

import TabItem from "@theme/TabItem";

<MarkdownChipRow labels={["Beginner", "Getting started"]} />

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />

Motoko is a modern, **actor-based** language designed for safety, efficiency, and scalability. Motoko natively supports asynchronous messaging, Candid interoperability, robust type safety, automatic memory management, and the unique features of the Internet Computer.

## Install Motoko

Follow these steps to set up your Motoko development environment:

### 1. **Install the IC SDK**  

To develop Motoko applications, you need a development environment that includes the Motoko compiler and base library. The Internet Computer Software Development Kit (IC SDK) provides these essential tools, along with utilities for managing and deploying canisters.  

```bash
sh -ci "$(curl -fsSL https://internetcomputer.org/install.sh)"
```

Verify the installation with:

```bash
dfx --version
```

### 2. **Choose an IDE**  

While you can use any IDE for Motoko development, **Visual Studio Code (VS Code)** is recommended due to its convenient extension support. If you prefer to use VS Code, you can download it from:  
[https://code.visualstudio.com](https://code.visualstudio.com)  

### 3. **Install the Motoko Extension(Optional, Recommended for VS Code Users)**

If you are using VS Code, you can enhance your development experience by installing the official Motoko extension:  

- Open VS Code  
- Go to Extensions (`Ctrl+Shift+X` / `Cmd+Shift+X`)  
- Search for **"Motoko"**  
- Install the official Motoko extension  

## Explore Motoko use cases

<Tabs>

<TabItem value="usecase1" label="Web applications" default>

Motoko makes it easy to build scalable backend services for web applications.

1. [Deploy your first Motoko canister.](https://github.com/dfinity/examples/tree/master/motoko/counter)

2. [Expand your application by calling external APIs from a Motoko canister.](https://github.com/dfinity/examples/tree/master/motoko/send_http_get)

3. [Explore a Reversi game built in motoko.](https://github.com/ninegua/reversi)  

</TabItem>

<TabItem value="usecase2" label="DeFi" default>

Build decentralized finance (DeFi) applications using Motoko.

1. [Set up an ICRC-1 canister.](https://github.com/sonicdex/icrc-1-public/)

2. [Create a decentralized exchange (DEX)](https://github.com/dfinity/examples/tree/master/motoko/icrc2-swap)  

3. [Learn how to deploy NFTs on ICP](https://github.com/noku-team/icrc7_motoko)

</TabItem>

<TabItem value="usecase3" label="Chain Fusion" default>

Develop cross-chain applications and integrate with other blockchains.  

1. [Explore the Ethereum integration.](https://github.com/dfinity/icp-eth-starter)

2. [Build a Bitcoin POS application.](https://github.com/dfinity/examples/tree/master/motoko/ic-pos)

</TabItem>

</Tabs>

## Quick references  

For developers looking for quick access to key resources, here are direct links to the base library and useful examples:

- **[Motoko base library](https://internetcomputer.org/docs/current/motoko/main/base/)**

- **[Motoko example projects](https://github.com/dfinity/examples/tree/master/motoko)**

### Share your feedback

Was this page helpful? [Yes] [No]

If you have any questions or suggestions, join the [DFINITY developer forum](https://forum.dfinity.org/) to share your feedback. Connect with the community, collaborate with other developers, and stay up to date with the latest Motoko updates.
