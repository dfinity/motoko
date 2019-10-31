type List<T> = ?{head : T; var tail : List<T>};

type Subscription = {
  post : shared Text -> ();  // revokable by Server
  cancel : shared () -> ();
};

type ClientData = {
  id : Nat;
  client : Client;
  var revoked : Bool;
};

actor class Server() = {
  var nextId : Nat = 0;
  var clients : List<ClientData> = null;

  func broadcast(id : Nat, message : Text) {
    var next = clients;
    label sends loop {
      switch next {
        case null { break sends };
        case (?n) {
          if (n.head.id != id) n.head.client.send(message);
          next := n.tail;
        };
      };
    };
  };

  public func subscribe(aclient : Client) : async Subscription {
    let c = {id = nextId; client = aclient; var revoked = false};
    nextId += 1;
    let cs = {head = c; var tail = clients};
    clients := ?cs;
    return object {
      public shared func post(message : Text) {
        if (not c.revoked) broadcast(c.id, message);
      };
      public shared func cancel() { unsubscribe(c.id) };
    };
  };

  func unsubscribe(id : Nat) {
    var prev : List<ClientData> = null;
    var next = clients;
    loop {
      switch next {
        case null return;
        case (?n) {
          if (n.head.id == id) {
            switch prev {
              case null { clients := n.tail };
              case (?p) { p.tail := n.tail };
            };
            print "(unsubscribe "; printNat id; print ")\n";
            return;
          };
          prev := next;
          next := n.tail;
        };
      };
    };
  };
};

actor class Client() = this {
  // TODO: these should be constructor params once we can compile them
  var name : Text = "";
  var server : ?Server  = null;

  public func go(n : Text, s : Server) {
    name := n;
    server := ?s;
    ignore(async {
      let sub = await s.subscribe(this);
      sub.post("hello from " # name);
      sub.post("goodbye from " # name);
      sub.cancel();
    })
  };

  public func send(msg : Text) {
    print(name # " received " # msg # "\n");
  };
};

let server = Server();
let bob = Client();
let alice = Client();
let charlie = Client();
bob.go("bob", server);
alice.go("alice", server);
charlie.go("charlie", server);
