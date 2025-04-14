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
    let request = {
      url = "https://ic0.app";
      method = #get;
      headers = [];
      body = null;
      transform = null;
      max_response_bytes = null;
    };
    let requestSize : Nat64 = 0;
    let defaultMaxResBytes : Nat64 = 2_000_000; // default when max_response_bytes is null
    print(debug_show (Prim.costHttpRequest(requestSize, defaultMaxResBytes)) # " -- http cost request");

    if (Cycles.balance() < 200_000_000_000) {
      await Cycles.provisional_top_up_actor(client, 3_000_000_000_000);
      print("top up; balance = " # debug_show (Cycles.balance()));
    } else {
      print("already topped up; balance = " # debug_show (Cycles.balance()));
    };

    printCycles();
    let response = await (with cycles = 200_000_000_000) ic00.http_request(request);
    print("response: " # debug_show (response));
    printCycles();
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
