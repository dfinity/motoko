// A simple data structure: mutable, singly linked list
type List<T> = ?{head : T; var tail : List<T>};

type Subscription = {
  post : shared Text -> ();  // revokable by Server
  cancel : shared () -> ();
};

type IClient = actor {
  send : shared Text -> ();
};

type IServer = actor {
  subscribe : IClient -> async Subscription;
};

type client = {
  id : Nat;
  client : IClient;
  var revoked : Bool;
};

actor Server = {
  private var nextId : Nat = 0;
  private var clients : List<client> = null;

  private broadcast(id : Nat, message : Text) {
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

  subscribe(iclient : IClient) : async Subscription {
    let c = new {id = nextId; client = iclient; var revoked = false};
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
    var prev : List<client> = null;
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

actor class Client() = this {
  // TODO: these should be constructor params once we can compile them
  private var name : Text = "";
  private var server : ?IServer  = null;

  go(n : Text, s : IServer) {
    name := n;
    server := ?s;
    ignore(async {
      let sub = await s.subscribe(this);
      sub.post("hello from " # name);
      sub.post("goodbye from " # name);
      sub.cancel();
    })
  };

  send(msg : Text) {
    print(name # " received " # msg # "\n");
  };
};

let bob = Client();
let alice = Client();
let charlie = Client();
bob.go("bob", Server);
alice.go("alice", Server);
charlie.go("charlie", Server);
