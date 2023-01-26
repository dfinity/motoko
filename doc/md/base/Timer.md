# Timer
Timers for one-off or periodic tasks.

Note: If `moc` is invoked with `-no-timer`, the importing will fail.
Note: The resolution of the timers is in the order of the block rate,
      so durations should be chosen well above that. For frequent
      canister wake-ups the heatbeat mechanism should be considered.

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
func deleteAppt(appt : Appointment) {
  cancelTimer (appt.reminder);
  // ...
};
```
