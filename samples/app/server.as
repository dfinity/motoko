import L "list.as";
import T "types.as";

type ClientData = {
  id : Nat;
  client : shared Text -> ();
  var revoked : Bool;
};

actor class Server() = {
  private var nextId : Nat = 0;
  private var clients : L.List<ClientData> = null;

  private broadcast(id : Nat, message : Text) {
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

  subscribe(aclient : shared Text -> ()) : async T.Subscription {
    let c = new {id = nextId; client = aclient; var revoked = false};
    nextId += 1;
    let cs = new {head = c; var tail = clients};
    clients := ?cs;
    return (new {
      post = shared func(message : Text) {
        if (not c.revoked) broadcast(c.id, message);
      };
      cancel = shared func() { unsubscribe(c.id) };
    });
  };

  private unsubscribe(id : Nat) {
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


