//MOC-FLAG --enhanced-orthogonal-persistence --default-persistent-actors --enhanced-migration

import Prim "mo:prim";

import Migration1 "Migration1";
import Migration2 "Migration2";
import Migration3 "Migration3";
import Migration4 "Migration4";
import Migration5 "Migration5";

// Bad ordering, will error out.
(
  with multi_migration = (
    Migration1.run,
    Migration5.run,
    Migration4.run,
    Migration2.run,
    Migration3.run,
  )
)
actor {
  var zero : Nat;

  var three : [var (Nat, Text)];

  var four : Text;

  var five : Text;

  var six : Text;

  public func check() : async () {
    Prim.debugPrint(debug_show { zero; three; four; five; six });
  };
};
