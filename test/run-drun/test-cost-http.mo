import Prim "mo:⛔";

import Cycles "cycles/cycles";

actor client {
  func print(t : Text) = Prim.debugPrint("client: " # t);

  public type http_header = { value : Text; name : Text };
  public type http_response = {
    status : Nat;
    body : [Nat8];
    headers : [http_header];
  };
  let ic00 = actor "aaaaa-aa" : actor {
    http_request : shared {
      url : Text;
      method : { #get; #head; #post };
      max_response_bytes : ?Nat64;
      body : ?[Nat8];
      transform : ?{
        function : shared query {
          context : [Nat8];
          response : http_response;
        } -> async http_response;
        context : [Nat8];
      };
      headers : [http_header];
    } -> async http_response;
  };

  public func go() : async () {
    // print(debug_show (Prim.costHttpGet()) # " -- http get cost");

    if (Cycles.balance() == 0) {
      await Cycles.provisional_top_up_actor(client, 3_000_000_000_000);
      print("top up; balance = " # debug_show (Cycles.balance()));
    } else {
      print("already topped up; balance = " # debug_show (Cycles.balance()));
    };

    // let before = Cycles.balance();
    printCycles();
    let response = await (with cycles = 200_000_000_000) ic00.http_request({
      url = "https://ic0.app";
      method = #get;
      headers = [];
      body = null;
      transform = null;
      max_response_bytes = null;
    });
    print("response: " # debug_show (response));
    printCycles();

    // print(debug_show (before) # " -- Cycles.balance() before");
    // print(debug_show (after) # " -- Cycles.balance() after");
    // print(debug_show (before - after : Nat) # " -- Cycles.balance() diff");
  };

  func printCycles() {
    print("Cycles.balance()   = " # debug_show (Cycles.balance()));
    print("Cycles.available() = " # debug_show (Cycles.available()));
    print("Cycles.refunded()  = " # debug_show (Cycles.refunded()));
  };
};

client.go(); //OR-CALL ingress go "DIDL\x00\x00"

//SKIP run
//SKIP run-ir
//SKIP run-low
