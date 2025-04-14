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
  public type http_request = {
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
  };
  let ic00 = actor "aaaaa-aa" : actor {
    http_request : shared http_request -> async http_response;
  };

  // Local replica output:
  // 15 -- calculated request size
  // 1_603_066_000 -- http cost request
  // 1_603_060_000 -- http cost request (with requestSize = 0)
  // 1_603_260_000 -- http cost request (with requestSize = 500)
  // 1_603_660_000 -- http cost request (with requestSize = 1500)
  // 2_003_060_000 -- http cost request (with requestSize = 1_000_000)
  // already topped up; balance = 3_058_372_523_503
  // Cycles.balance()   = 3_058_372_523_503
  // Cycles.available() = 0
  // Cycles.refunded()  = 0
  // response: {body = []; headers = [{name = "location"; value = "https://dashboard.internetcomputer.org/"}, {name = "strict-transport-security"; value = "max-age=31536000; includeSubDomains"}, {name = "x-request-id"; value = "019633af-09b9-78e2-bb9e-92fd2d3c0a95"}, {name = "content-length"; value = "0"}, {name = "date"; value = "Mon, 14 Apr 2025 09:43:50 GMT"}]; status = 307}
  // Cycles.balance()   = 3_056_768_993_225
  // Cycles.available() = 0
  // Cycles.refunded()  = 198_396_934_000
  // 1_603_530_278 -- Cycles.balance() diff
  public func go() : async () {
    let request = {
      url = "https://ic0.app";
      method = #get;
      headers = [];
      body = null;
      transform = null;
      max_response_bytes = null;
    };
    let requestSize : Nat64 = calculateRequestSize(request);
    print(debug_show (requestSize) # " -- calculated request size");
    let defaultMaxResBytes : Nat64 = 2_000_000; // default when max_response_bytes is null
    print(debug_show (Prim.costHttpRequest(requestSize, defaultMaxResBytes)) # " -- http cost request");
    print(debug_show (Prim.costHttpRequest(0, defaultMaxResBytes)) # " -- http cost request (with requestSize = 0)");
    print(debug_show (Prim.costHttpRequest(500, defaultMaxResBytes)) # " -- http cost request (with requestSize = 500)");
    print(debug_show (Prim.costHttpRequest(1500, defaultMaxResBytes)) # " -- http cost request (with requestSize = 1500)");
    print(debug_show (Prim.costHttpRequest(1_000_000, defaultMaxResBytes)) # " -- http cost request (with requestSize = 1_000_000)");

    if (Cycles.balance() < 200_000_000_000) {
      await Cycles.provisional_top_up_actor(client, 3_000_000_000_000);
      print("top up; balance = " # debug_show (Cycles.balance()));
    } else {
      print("already topped up; balance = " # debug_show (Cycles.balance()));
    };

    let before = Cycles.balance();
    printCycles();
    let response = await (with cycles = 200_000_000_000) ic00.http_request(request);
    let after = Cycles.balance();
    print("response: " # debug_show (response));
    printCycles();
    print(debug_show (before - after : Nat) # " -- Cycles.balance() diff");
  };

  func printCycles() {
    print("Cycles.balance()   = " # debug_show (Cycles.balance()));
    print("Cycles.available() = " # debug_show (Cycles.available()));
    print("Cycles.refunded()  = " # debug_show (Cycles.refunded()));
  };

  /// [Source](https://github.com/dfinity/cdk-rs/pull/570/files#diff-c415c9ec7f29503dcb76e31fcac3062b0311cc0705f9159c1dfe3cd3308957b7R425)
  func calculateRequestSize(request : http_request) : Nat64 {
    var size : Nat64 = 0;
    
    // Add URL byte length
    size += Prim.natToNat64(request.url.size());
    
    // Add headers byte length (sum of all names and values)
    for (header in request.headers.vals()) {
      size += Prim.natToNat64(header.name.size());
      size += Prim.natToNat64(header.value.size());
    };
    
    // Add body length if present
    switch (request.body) {
      case (?body) { size += Prim.natToNat64(body.size()) };
      case null {};
    };
    
    // Add transform context length if present
    switch (request.transform) {
      case (?transform) {
        size += Prim.natToNat64(transform.context.size());
      };
      case null {};
    };
    
    size
  };
};

client.go(); //OR-CALL ingress go "DIDL\x00\x00"

//SKIP run
//SKIP run-ir
//SKIP run-low
