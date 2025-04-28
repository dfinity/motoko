---
sidebar_position: 2
---

# Optimizing canisters



The Motoko compiler produces small binaries with reasonably efficient code, but is not a highly optimized compiler.
It is possible to further optimize Motoko binaries, both for code size and cycle usage, using additional tools such as `wasm-opt`.

## Using `wasm-opt`

`Wasm-opt` is a general purpose Wasm optimizer that is now available in dfx, versions 0.14.0 and newer.

`Wasm-opt` can be used to enable canister optimizations through a configuration option in the project's `dfx.json` file, such as:

```json
{
  "canisters": {
    "my_canister": {
      "optimize": "cycles"
    }
  }
}
```

### Optimization levels for cycle usage

Using the `"optimize": "cycles"` option, you can expect a rough estimate of decreased cycles usage for Motoko canisters by around 10%.

The `"optimize": "cycles"` option is the recommended default, as it maps to optimization level 3 in the `wasm-opt` package.

The optimization levels for cycles usage are as follows:

```
O4
O3 (equivalent to “cycles”)
O2
O1
O0 (performs no optimizations)
```

### Optimization levels for binary size

To optimize the binary size instead, you can use the `"optimize": "size"` option. By using the size option, binary sizes can be reduced by roughly 16%.

The optimization levels for binary size are as follows:

```
Oz (equivalent to “size”)
Os
```

Each optimization preserves the Internet Computer specific metadata sections of each canister.

:::info
Note that in certain cases the optimizations can increase the complexity of certain functions in your Wasm module such that they are rejected by the replica. If you run into this issue, it is recommended to use a less aggressive optimization level such that you do not exceed the complexity limit.
:::

More information on canister optimization and information on `wasm-opt` benchmark testing can be found [on this forum post](https://forum.dfinity.org/t/canister-optimizer-available-in-dfx-0-14-0/21157).


<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />
