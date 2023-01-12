import Debug "mo:base/Debug";
import { abs } "mo:base/Int";
import { now } "mo:base/Time";
import { setTimer; recurringTimer } "mo:base/Timer";

actor Reminder {

  let solarYearNanos = 356_925_216_000_000;

  private func remind() : async () {
    Debug.print("Happy New Year!");
  };

  ignore setTimer(#nanoseconds (abs(now() % solarYearNanos)), func () : async () {
    ignore recurringTimer(#nanoseconds solarYearNanos, remind);
    await remind();
  });
}
