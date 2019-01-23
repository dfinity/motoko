// A simple data structure: mutable, singly linked list
type List<T> = {head : T; var tail : List<T>}?;

type Subscription = {
  post : shared Text -> async ();  // revokable by Server
  cancel : shared () -> ();
};

type IClient = actor {
   send : shared Text -> async ();
};

type IServer = actor {
  subscribe : IClient -> async Subscription;
};

type client = {id : Nat; client : IClient; var revoked : Bool};

actor Server {
  private var nextId : Nat = 0;
  private var clients : List<client> = null;

  private broadcast(id : Nat, message : Text) : async () {
    var next = clients;
    var replies : List<async ()> = null;
    label sends loop {
      switch next {
      	case null { break sends };
	      case (n?) {
          if (n.head.id != id) {
 	          let reply = n.head.client.send(message);
		        replies := (new {head = reply; var tail = replies})?;
		      };
		      next := n.tail;
        };
      };
    };
    loop {
      switch replies {
      	case null return;
  	    case (r?) {
          await r.head;
		      replies := r.tail;
        };
      };
    };
  };

  subscribe(iclient : IClient) : async Subscription {
    let c = new {id = nextId; client = iclient; var revoked = false};
    nextId += 1;
    let cs = new {head = c; var tail = clients};
    clients := cs?;
    return (new {
      post = (shared func(message : Text) : async () {
       	if (not c.revoked) await broadcast(c.id, message);
		  });
      cancel = (shared func() { unsubscribe(c.id) });
    });
  };

  private unsubscribe(id : Nat) {
    var prev : List<client> = null;
    var next = clients;
    loop {
      switch next {
    	  case null return;
	      case (n?) {
	        if (n.head.id == id) {
		        switch prev {
		          case null { clients := n.tail };
		          case (p?) { p.tail := n.tail };
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
  private var server: IServer?  = null;

  go(n : Text, s : IServer) {
    name := n;
    server := s?;
    ignore(async {
      let sub = await s.subscribe(this);
      await sub.post("hello from " # name);
      await sub.post("goodbye from " # name);
      sub.cancel();
    })
  };

  send(msg : Text) : async () {
    print(name # " received " # msg # "\n");
  };
};

let bob = Client();
let alice = Client();
let charlie = Client();
bob.go("bob", Server);
alice.go("alice", Server);
charlie.go("charlie", Server);
