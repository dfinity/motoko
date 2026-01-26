//MOC-FLAG --enhanced-orthogonal-persistence --default-persistent-actors --enhanced-migration

import Prim "mo:prim";

import Migration "Migration6";

(
  with multi_migration = (
    Migration.run
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
