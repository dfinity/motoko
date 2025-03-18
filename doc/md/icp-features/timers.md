---
sidebar_position: 2
---

# Timers

Canisters on the Internet Computer can schedule code execution at specific intervals using timers. [Timers](https://internetcomputer.org/docs/motoko/main/base/Timer) enable periodic tasks such as recurring state mutation, automating maintenance, or enforcing timed access control. The Motoko standard library provides the `Timer` module, which allows setting up **recurring timers** to execute code at regular intervals. These timers are managed at the application level and can be used for tasks like rate limiting or periodic updates. For more precise control, Motoko also supports **system timers**, which interact with the canister’s global timer mechanism. System timers operate at a lower level, allowing a canister to handle timer expirations directly by defining a `system func timer()`. While recurring timers provide a straightforward approach for most time-based tasks, system timers offer finer control over scheduling but may interfere with the `Timer` module if both are used together.

## Recurring timers

The `Timer.recurringTimer` function enables canisters to execute code at fixed intervals, making it useful for automating tasks that must run repeatedly. One practical application is **rate limiting**, where a canister restricts the number of requests it processes within a specific time window. This prevents excessive usage and ensures fair access to resources. By tracking request counts and rejecting requests beyond a set limit, a canister can enforce controlled access. A recurring timer resets the counter automatically at regular intervals, restoring access when the time window expires. In the following example, a canister limits requests to five per minute, rejecting additional requests until the counter resets:

```motoko no-repl
import Timer "mo:base/Timer";
import Error "mo:base/Error";

persistent actor {
  var requestCount : Nat = 0;
  let maxRequests : Nat = 5;

  // Reset request count every 60 seconds
  //It must be ignored because it produces a TimerId
  ignore Timer.recurringTimer<system>(
    #seconds 60,
    func() : async () {
      requestCount := 0
    }
  );

  public func request() : async Text {
    if (requestCount >= maxRequests) {
      throw Error.reject("Rate limit exceeded. Try again later.")
    };
    requestCount += 1;
    "Request accepted."
  }
}
```

## System timers

System timers offer low-level control over scheduled execution by interfacing directly with the canister’s global timer. Unlike `Timer.recurringTimer`, which executes code at fixed intervals, system timers allow scheduling execution at a precise expiration time. One practical use case of system timers is **auto-expiring access tokens**, where a token is issued for a fixed duration and revoked automatically without requiring an external request. This approach is useful for **session management, expiring permissions, or enforcing time-based access controls** where precise expiration is required. The following example sets a system timer to revoke an access token exactly 30 minutes after it is issued:

```motoko no-repl
import Time "mo:base/Time";
import Nat "mo:base/Nat";
import Int "mo:base/Int";
import Nat64 "mo:base/Nat64";
import Debug "mo:base/Debug";

persistent actor {
   var accessToken : ?Text = null;

  public func issueToken() : async Text {
    let token = "user-token-" # Nat.toText(Int.abs(Time.now()));
    accessToken := ?token;

    token
  };
  // Set a system timer to revoke the token after 30 minutes
  system func timer(setGlobalTimer : Nat64 -> ()) : async () {
    let expiryTime = Nat64.fromNat(Int.abs(Time.now()) + 1_800_000_000_000); // 30 minutes in nanoseconds
    setGlobalTimer(expiryTime);
    accessToken := null;
    Debug.print("Access token expired.")
  };
  public query func validateToken() : async Bool {
    accessToken != null
  }
}

```

## References

- [Timer](https://internetcomputer.org/docs/motoko/main/base/Timer)
- [Time](https://internetcomputer.org/docs/motoko/main/base/Time)
