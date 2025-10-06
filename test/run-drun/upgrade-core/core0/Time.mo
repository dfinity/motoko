/// System time utilities and timers.
///
/// The following example illustrates using the system time:
///
/// ```motoko
/// import Int = "mo:core/Int";
/// import Time = "mo:core/Time";
///
/// persistent actor {
///   var lastTime = Time.now();
///
///   public func greet(name : Text) : async Text {
///     let now = Time.now();
///     let elapsedSeconds = (now - lastTime) / 1000_000_000;
///     lastTime := now;
///     return "Hello, " # name # "!" #
///       " I was last called " # Int.toText(elapsedSeconds) # " seconds ago";
///    };
/// };
/// ```
///
/// Note: If `moc` is invoked with `-no-timer`, the importing will fail.
/// Note: The resolution of the timers is in the order of the block rate,
///       so durations should be chosen well above that. For frequent
///       canister wake-ups the heartbeat mechanism should be considered.

import Types "Types";
import Nat "Nat";
import Prim "mo:⛔";

module {

  /// System time is represent as nanoseconds since 1970-01-01.
  public type Time = Types.Time;

  /// Quantity of time expressed in `#days`, `#hours`, `#minutes`, `#seconds`, `#milliseconds`, or `#nanoseconds`.
  public type Duration = Types.Duration;

  /// Current system time given as nanoseconds since 1970-01-01. The system guarantees that:
  ///
  /// * the time, as observed by the canister smart contract, is monotonically increasing, even across canister upgrades.
  /// * within an invocation of one entry point, the time is constant.
  ///
  /// The system times of different canisters are unrelated, and calls from one canister to another may appear to travel "backwards in time"
  ///
  /// Note: While an implementation will likely try to keep the system time close to the real time, this is not formally guaranteed.
  public let now : () -> Time = func() : Int = Prim.nat64ToNat(Prim.time());

  public type TimerId = Nat;

  public func toNanoseconds(duration : Duration) : Nat {
    switch duration {
      case (#days n) n * 86_400_000_000_000;
      case (#hours n) n * 3_600_000_000_000;
      case (#minutes n) n * 60_000_000_000;
      case (#seconds n) n * 1_000_000_000;
      case (#milliseconds n) n * 1_000_000;
      case (#nanoseconds n) n
    }
  };

}
