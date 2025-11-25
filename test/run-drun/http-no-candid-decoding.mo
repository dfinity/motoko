import Prim "mo:prim";

actor {

  public shared query ({ caller }) func http_handler_query(b : [Nat8]) : async [Nat8] {
    Prim.debugPrint("http_handler_query: " # debug_show (b));
    Prim.debugPrint("caller: " # debug_show (caller));
    b;
  };

  public shared ({ caller }) func http_handler_update(b : Blob) : async Blob {
    Prim.debugPrint("http_handler_update: " # debug_show (b));
    Prim.debugPrint("caller: " # debug_show (caller));
    b;
  };

  public shared func test1(v : [Nat8]) : async [Nat8] {
    Prim.debugPrint("test1: " # debug_show (v));
    v;
  };

};

//SKIP run
//SKIP run-ir
//SKIP run-low
//ENHANCED-ORTHOGONAL-PERSISTENCE-ONLY
//MOC-NO-FORCE-GC

//CALL query http_handler_query "\x11\x12\x13"
//CALL ingress http_handler_update "\x05\x06\x07\x08\x09"
//CALL query http_handler_query "__test"
//CALL ingress test1 "DIDL\x01\x6d\x7b\x01\x00\x03\x05\x06\x07"

//CALL ingress http_handler_update "\x00\x03\x47\x45\x54\x05\x68\x74\x74\x70\x73\x36\x17\x68\x70\x73\x2d\x6d\x79\x61\x61\x61\x2d\x61\x61\x61\x61\x75\x2d\x61\x63\x75\x6e\x61\x2d\x63\x61\x69\x2e\x69\x63\x70\x33\x2e\x69\x6f\x0a\x2f\x61\x70\x69\x2f\x74\x6f\x64\x6f\x73\x26\x12\x69\x63\x2d\x69\x6e\x63\x6c\x75\x64\x65\x2d\x68\x65\x61\x64\x65\x72\x73\x12\x69\x63\x2d\x69\x6e\x63\x6c\x75\x64\x65\x2d\x68\x65\x61\x64\x65\x72\x73\x00\x00"
