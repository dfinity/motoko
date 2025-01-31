---
sidebar_position: 4
---


# Motoko quickstart



This quickstart guide showcases how to deploy a simple 'Hello, world!' Motoko smart contract.

## Prerequisites

Before getting started, assure you have set up your developer environment according to the instructions in the [developer environment guide](./dev-env).

## Creating a new project

Open a terminal window on your local computer, if you don’t already have one open.

Create a new project and change to the project directory.

Use `dfx new [project_name]` to create a new project:

```
dfx new hello_world
```

You will be prompted to select the language that your backend canister will use:

```
? Select a backend language: ›
❯ Motoko
Rust
TypeScript (Azle)
Python (Kybra)
```

Then, select a frontend framework for your frontend canister. In this example, select:

```
? Select a frontend framework: ›
SvelteKit
React
Vue
Vanilla JS
No JS template
❯ No frontend canister
```

Lastly, you can include extra features to be added to your project:

```
? Add extra features (space to select, enter to confirm) ›
⬚ Internet Identity
⬚ Bitcoin (Regtest)
⬚ Frontend tests
```

## Smart contract code

This hello world actor has a single function called `greet`. It is marked as `query` because it doesn't modify the state of the actor. The function accepts a name as input in type [`Text`](../base/Text.md) and returns a greeting of type [`Text`](../base/Text.md).


```motoko title="src/hello_backend/main.mo"

actor {
  public query func greet(name : Text) : async Text {
    return "Hello, " # name # "!";
  };
};
```

## Starting the deployment environment

Start the Internet Computer for local development or check your connection to the Internet Computer for network deployment:
- [Local deployment](https://internetcomputer.org/docs/current/developer-docs/getting-started/deploy-and-manage).
- [Mainnet deployment](https://internetcomputer.org/docs/current/developer-docs/getting-started/deploy-and-manage).

## Register, build, and deploy locally or on the mainnet

To deploy locally, use the command:

```
dfx deploy
```

For deploying to the mainnet, use: `--network ic`.

```
dfx deploy --network <network>
```

## View your service or application in a browser, using the URLS in the output of the `dfx deploy` command:

```
...
Committing batch.
Committing batch with 18 operations.
Deployed canisters.
URLs:
Frontend canister via browser
        access_hello_frontend: http://127.0.0.1:4943/?canisterId=cuj6u-c4aaa-aaaaa-qaajq-cai
Backend canister via Candid interface:
        access_hello_backend: http://127.0.0.1:4943/?canisterId=cbopz-duaaa-aaaaa-qaaka-cai&id=ctiya-peaaa-aaaaa-qaaja-cai
```

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />
