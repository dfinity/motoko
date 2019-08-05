import L "list.as";
import T "types.as";

type ClientData = {
  id : Nat;
  client : shared Text -> ();
  var revoked : Bool;
};

actor class Server() = {
  var nextId : Nat = 0;
  var clients : L.List<ClientData> = null;

  func broadcast(id : Nat, message : Text) {
    var next = clients;
    label sends loop {
      switch next {
        case null { break sends };
        case (?n) {
          if (n.head.id != id) n.head.client(message);
          next := n.tail;
        };
      };
    };
  };

  public func subscribe(aclient : shared Text -> ()) : async T.Subscription {
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
    var prev : L.List<ClientData> = null;
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
            print "(unsubscribe "; printInt id; print ")\n";
            return;
          };
          prev := next;
          next := n.tail;
        };
      };
    };
  };
};


