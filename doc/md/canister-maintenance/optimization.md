# Optimizing canisters

## Overview

Optimizing canisters is an important development step that can assure your canister is using the most efficient workflow by optimizing the canister's cycles usage and binary size.  This guide covers how to optimize Motoko canisters.

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
