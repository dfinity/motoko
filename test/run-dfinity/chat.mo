type List<T> = ?{head : T; var tail : List<T>};

type Post = shared Text -> ();

actor class Server() = {
  var clients : List<Client> = null;

  shared func broadcast(message : Text) {
    var next = clients;
    loop {
      switch next {
        case null return;
        case (?l) {
          l.head.send(message);
          next := l.tail;
        };
      };
    };
  };

  public func subscribe(client : Client) : async Post {
    let cs = {head = client; var tail = clients};
    clients := ?cs;
    return broadcast;
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
      let post = await s.subscribe(this);
      post("hello from " # name);
      post("goodbye from " # name);
    });
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


/* design flaws:
     - we can't (synchronously) subscribe in the Client constructor as its async, need a separate 'go' method.
   tc: reordering IServer and IClient leads to a complaint that subscribe has non-sharable argument type - we probably need to normalize while checking.
   compiler:
     - parameterising Client on s:IServer argument complains about non-closed actor (expected acc. to Joachim, pending system changes)
*/
   
