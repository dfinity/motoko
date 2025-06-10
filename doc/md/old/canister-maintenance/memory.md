---
sidebar_position: 5
---

# Memory diagnostics

## Low memory hook

The IC allows to implement a low memory hook, which is a warning trigger when main memory is becoming scarce.

For this purpose, a Motoko actor or actor class instance can implement the system function `lowmemory()`. This system function is scheduled when canister's free main memory space has fallen below the defined threshold `wasm_memory_threshold`, that is is part of the canister settings. In Motoko, `lowmemory()` implements the `canister_on_low_wasm_memory` hook defined in the IC specification.

Example of using the low memory hook:
```
actor {
    system func lowmemory() : async* () {
        Debug.print("Low memory!");
    }
}
```

The following properties apply to the low memory hook:
* The execution of `lowmemory` happens with a certain delay, as it is scheduled as a separate asynchronous message that runs after the message in which the threshold was crossed.
* Once executed, `lowmemory` is only triggered again when the main memory free space first exceeds and then falls below the threshold.
* Traps or unhandled errors in `lowmemory` are ignored. Traps only revert the changes done in `lowmemory`.
* Due to its `async*` return type, the `lowmemory` function may send further messages and `await` results.
