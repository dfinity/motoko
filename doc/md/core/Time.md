# core/Time
System time utilities and timers.

The following example illustrates using the system time:

```motoko
import Int = "mo:core/Int";
import Time = "mo:core/Time";

persistent actor {
  var lastTime = Time.now();

  public func greet(name : Text) : async Text {
    let now = Time.now();
    let elapsedSeconds = (now - lastTime) / 1000_000_000;
    lastTime := now;
    return "Hello, " # name # "!" #
      " I was last called " # Int.toText(elapsedSeconds) # " seconds ago";
   };
};
```

Note: If `moc` is invoked with `-no-timer`, the importing will fail.
Note: The resolution of the timers is in the order of the block rate,
      so durations should be chosen well above that. For frequent
      canister wake-ups the heartbeat mechanism should be considered.

## Type `Time`
``` motoko no-repl
type Time = Types.Time
```

System time is represent as nanoseconds since 1970-01-01.

## Type `Duration`
``` motoko no-repl
type Duration = Types.Duration
```

Quantity of time expressed in `#days`, `#hours`, `#minutes`, `#seconds`, `#milliseconds`, or `#nanoseconds`.

## Function `now`
``` motoko no-repl
func now() : Time
```

Current system time given as nanoseconds since 1970-01-01. The system guarantees that:

* the time, as observed by the canister smart contract, is monotonically increasing, even across canister upgrades.
* within an invocation of one entry point, the time is constant.

The system times of different canisters are unrelated, and calls from one canister to another may appear to travel "backwards in time"

Note: While an implementation will likely try to keep the system time close to the real time, this is not formally guaranteed.

## Type `TimerId`
``` motoko no-repl
type TimerId = Nat
```


## Function `toNanoseconds`
``` motoko no-repl
func toNanoseconds(duration : Duration) : Nat
```

