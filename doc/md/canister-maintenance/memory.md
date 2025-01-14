---
sidebar_position: 5
---

# Memory management

## Low memory hook

The IC allows to implement a low memory hook, which is a warning trigger when main memory is becoming scarce.

For this purpose, a Motoko actor or actor class instance can implement the system function `onLowMemory()`. This system function is scheduled when canister's free main memory space has fallen below the defined threshold `wasm_memory_threshold`, that is is part of the canister settings. In Motoko, `onLowMemory()` implements the `canister_on_low_wasm_memory` hook defined in the IC specification.

Example of using the low memory hook:
```
actor {
    system func onLowMemory() : async () {
        Debug.print("Low memory!");
    }
}
```

A few aspects need to be considered with the low memory hook:
* The execution of `onLowMemory` happens with a certain delay, as it is scheduled as separate asynchronous message that runs after the message in which the threshold was crossed.
* Once executed, `onLowMemory` is only triggered again when the main memory free space went above the threshold (e.g. by lowering the threshold or shrinking the main memory through canister reinstallation) and when the free space again fell below the threshold.
* Traps or unhandled errors in `onLowMemory` are ignored. They only revert the changes done in `onLowMemory`.
* Due to its `async` return type, the `onLowMemory` function may send further messages and await results.
