import Prim = "mo:⛔";
// let-like annotation
actor This : actor { beep : () -> async () } {

  public func beep() : async () {};

  public func ohoh() : async () {Prim.debugPrint "ohoh"}; // still exported, despite not in declared interface!

  public func check() : async ()  { await This.ohoh() }; // check no curtailment this

}
//CALL ingress ohoh "DIDL\x00\x00"
