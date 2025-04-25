---
sidebar: 2
hide_table_of_contents: true
---

# Developer containers


Developer containers are a local development option that uses [Docker](https://www.docker.com/get-started/) and [VS Code](https://code.visualstudio.com/) to run local containerized environments. Containers are isolated from the rest of your local environment, and files within a container cannot be used by applications outside of the container unless explicitly mounted and given access. Developer containers are a good option for developers on Windows systems, since `dfx` is not natively supported for local development on Windows.

Dev containers have additional benefits, including:

- **Replicability**: Containers make reproducing builds across teams easy since they contain the entire development environment within a portable format. Team members can interact with the same container without needing to set up their own personal developer environment and reproduce the build themselves.

- **Security**: Containers do not have access to the other files or applications on your system, making project development secure.

## Using developer containers

To use developer containers, you must download and install:

- [Docker](https://www.docker.com/get-started/).

- [VS Code](https://code.visualstudio.com/).

- The [Dev Containers VS Code extension](https://containers.dev/supporting#dev-containers).

Make sure Docker is running, then navigate into your project's directory. Create a file called `devcontainer.json` that contains the following:

```json
{
  "name": "ICP Dev Environment",
  "image": "ghcr.io/dfinity/icp-dev-env-slim:13",
  "forwardPorts": [4943, 5173],
  "portsAttributes": {
    "4943": {
      "label": "dfx",
      "onAutoForward": "ignore"
    },
    "5173": {
      "label": "vite",
      "onAutoForward": "openBrowser"
    }
  },
  "customizations": {
    "vscode": {
      "extensions": ["dfinity-foundation.vscode-motoko"]
    }
  }
}
```

Then, start the Dev Container by opening the VS Code command palette (F1 or Ctrl+Shift+P), then select `Dev-Containers: Reopen in Container`.

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />
