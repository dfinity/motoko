# Timer
Timers for one-off or periodic tasks. Applicable as part of the default mechanism.
If `moc` is invoked with `-no-timer`, the importing will fail. Furthermore, if passed `--trap-on-call-error`, a congested canister send queue may prevent timer expirations to execute at runtime. It may also deactivate the global timer.

The resolution of the timers is similar to the block rate,
so durations should be chosen well above that. For frequent
canister wake-ups, consider using the [heartbeat](https://internetcomputer.org/docs/current/motoko/main/writing-motoko/heartbeats) mechanism; however, when possible, canisters should prefer timers.

The functionality described below is enabled only when the actor does not override it by declaring an explicit `system func timer`.

Timers are _not_ persisted across upgrades. One possible strategy
to re-establish timers after an upgrade is to use stable variables
in the `post_upgrade` hook and distill necessary timer information
from there.

Using timers for security (e.g., access control) is strongly discouraged.
Make sure to inform yourself about state-of-the-art dapp security.
If you must use timers for security controls, be sure
to consider reentrancy issues as well as the vanishing of timers on upgrades
and reinstalls.

For further usage information for timers on the IC, please consult
[the documentation](https://internetcomputer.org/docs/current/developer-docs/backend/periodic-tasks#timers-library-limitations).

## Type `Duration`
``` motoko no-repl
type Duration = {#seconds : Nat; #nanoseconds : Nat}
```


## Type `TimerId`
``` motoko no-repl
type TimerId = Nat
```


## Function `setTimer`
``` motoko no-repl
func setTimer(d : Duration, job : () -> async ()) : TimerId
```

Installs a one-off timer that upon expiration after given duration `d`
executes the future `job()`.

```motoko no-repl
let now = Time.now();
let thirtyMinutes = 1_000_000_000 * 60 * 30;
func alarmUser() : async () {
  // ...
};
appt.reminder = setTimer(#nanoseconds (Int.abs(appt.when - now - thirtyMinutes)), alarmUser);
```

## Function `recurringTimer`
``` motoko no-repl
func recurringTimer(d : Duration, job : () -> async ()) : TimerId
```

Installs a recurring timer that upon expiration after given duration `d`
executes the future `job()` and reinserts itself for another expiration.

Note: A duration of 0 will only expire once.

```motoko no-repl
func checkAndWaterPlants() : async () {
  // ...
};
let daily = recurringTimer(#seconds (24 * 60 * 60), checkAndWaterPlants);
```

## Value `cancelTimer`
``` motoko no-repl
let cancelTimer : TimerId -> ()
```

Cancels a still active timer with `(id : TimerId)`. For expired timers
and not recognised `id`s nothing happens.

```motoko no-repl
func deleteAppointment(appointment : Appointment) {
  cancelTimer (appointment.reminder);
  // ...
};
```
