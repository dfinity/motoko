import Prim = "mo:⛔";
// let-like annotation
actor Main : actor { beep : () -> async () } {

  public func beep() : async () {};

  public func ohoh() : async () {Prim.debugPrint "ohoh"}; // still exported, despite not in declared interface!

}
//CALL ingress ohoh "DIDL\x00\x00"
