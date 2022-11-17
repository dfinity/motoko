import Prim "mo:⛔";
actor this {

  public func ping () : async () {
    Prim.debugPrint("In ping(), outstanding callbacks: " # debug_show Prim.rts_callback_table_count());
  };

  public func go() : async () {
    Prim.debugPrint("In go(), outstanding callbacks: " # debug_show Prim.rts_callback_table_count());
    await this.ping();
    Prim.debugPrint("In go() again, outstanding callbacks: " # debug_show Prim.rts_callback_table_count());
    //Prim.trap("trapping now");
    assert(false);
  };

  public query func stats() : async () {
    Prim.debugPrint("In stats(), outstanding callbacks: " # debug_show Prim.rts_callback_table_count());
  };

  Prim.debugPrint ("init'ed");
}
//CALL ingress stats "DIDL\x00\x00"
//CALL ingress go "DIDL\x00\x00"
//CALL ingress stats "DIDL\x00\x00"
//CALL upgrade
//CALL ingress stats "DIDL\x00\x00"

//SKIP run
//SKIP run-ir
//SKIP run-low
