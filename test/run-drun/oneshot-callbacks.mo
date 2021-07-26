import Prim "mo:⛔";

actor a {

  public shared func oneway_ping() : () {
    Prim.debugPrint("ping! " # debug_show Prim.rts_callback_table_count());
  };

  public func ping() : async () {
    Prim.debugPrint("ping-async! " # debug_show Prim.rts_callback_table_count());
  };

  public func go(trigger_cleanup : Bool) : async () {
    Prim.debugPrint("go 0: " # debug_show Prim.rts_callback_table_count());
    oneway_ping();
    await async {Prim.debugPrint("go 1: " # debug_show Prim.rts_callback_table_count())};
    await ping();
    try {
        ignore await (async {Prim.debugPrint("go 2: " # debug_show Prim.rts_callback_table_count()); assert false; 42})
    } catch _ { if trigger_cleanup { ignore 42/0 } }
  };

  public func go_cleanup() : async () {
      await go(true)
  };

  public func go_normal() : async () {
      await go(false)
  };
};
await a.go_cleanup(); //OR-CALL ingress go_cleanup "DIDL\x00\x00"
await a.go_normal(); //OR-CALL ingress go_normal "DIDL\x00\x00"
await a.go_cleanup(); //OR-CALL ingress go_cleanup "DIDL\x00\x00"

//SKIP run
//SKIP run-low
//SKIP run-ir
