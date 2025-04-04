import { print } = "mo:base/Debug";
import { abs } = "mo:base/Int";
import { now } = "mo:base/Time";
import { setTimer; recurringTimer } = "mo:base/Timer";

persistent actor Reminder {

  transient let solarYearSeconds = 356_925_216;

  private func remind() : async () {
    print("Happy New Year!");
  };

  ignore setTimer<system>(#seconds (solarYearSeconds - abs(now() / 1_000_000_000) % solarYearSeconds),
    func () : async () {
      ignore recurringTimer<system>(#seconds solarYearSeconds, remind);
      await remind();
  });
}
