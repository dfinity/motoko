# Time
System time

## Type `Time`
``` motoko no-repl
type Time = Int
```

System time is represent as nanoseconds since 1970-01-01.

## Value `now`
``` motoko no-repl
let now : () -> Time
```

Current system time given as nanoseconds since 1970-01-01. The system guarantees that:

* the time, as observed by the canister smart contract, is monotonically increasing, even across canister upgrades.
* within an invocation of one entry point, the time is constant.

The system times of different canisters are unrelated, and calls from one canister to another may appear to travel "backwards in time"

Note: While an implementation will likely try to keep the system time close to the real time, this is not formally guaranteed.
