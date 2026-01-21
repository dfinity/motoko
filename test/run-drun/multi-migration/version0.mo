//MOC-FLAG --enhanced-orthogonal-persistence --default-persistent-actors --enhanced-migration

import Prim "mo:prim";
import Init "Init";

(with multi_migration = (Init.run))
actor {
  var zero : Nat;
  var one : [var Nat];
  var two : [var Text];

  public func check() : async () {
    Prim.debugPrint(debug_show "Version 0");
    Prim.debugPrint(debug_show { zero; one; two });
  };
};
