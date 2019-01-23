/* a simple data structure: mutable, singly linked list */
type List<T> = {head : T; var tail : List<T>}?;

type Post = shared Text -> async ();

type IClient = actor {
  send : Text -> async ();
};

type IServer = actor {
  subscribe : IClient -> async Post;
};

actor Server {
  private var clients : List<IClient> = null;

  private shared post(message : Text) : async () {
    var next = clients;
    loop {
      switch next {
        case null return;
	      case (l?) {
          await l.head.send(message);
          next := l.tail;
        };
      };
    };
  };

  subscribe(client : IClient) : async Post {
    let cs = new {head = client; var tail = clients};
    clients := cs?;
    return post;
  };
};


actor class Client() = this {
  // TODO: these should be constructor params once we can compile them
  private var name : Text = "";
  private var server : IServer?  = null;

  go(n : Text, s : IServer) {
    name := n;
    server := s?;
    ignore(async {
      let post = await s.subscribe(this);
      await post("hello from " # name);
      await post("goodbye from " # name);
    });
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

/* features to add:
   don't broadcast to sender
   subscribe should also return unsubscribe capability (use actor identity (if we support that) or add a tag)
   Client's 'do not disturb' that 'revokes' send by changing its behaviour.
*/   


/* design flaws:
     - it's annoying you can't await in a ()-message, just an awaitable one (eg. Client.go()).
     - we can't (synchronously) subscribe in the Client constructor as its async, need a separate 'go' method.
   tc: reordering IServer and IClient leads to a complaint that subscribe has non-sharable argument type - we probably need to normalize while checking.
   compiler:
     - parameterising Client on s:IServer argument complains about non-closed actor (expected acc. to Joachim, pending system changes)
*/
   
