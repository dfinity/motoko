import Prim "mo:prim";
import Migration1 "Migration1";
import Migration2 "Migration2";
import Migration3 "Migration3";
import Migration4 "Migration4";

(with multi_migration = (Migration1.run, Migration2.run, Migration3.run, Migration4.run))
actor {

  Prim.debugPrint("Version 1");

  stable var zero : Nat = 0;
  assert zero == 0;

  stable var three : [var (Nat, Text)] = [var];

  stable var four : Text = "";

  stable var five : Text = "";

  stable var six : Text = "";

  public func check() : async () {
    Prim.debugPrint(debug_show { zero; three; four; five; six });
  };
};
