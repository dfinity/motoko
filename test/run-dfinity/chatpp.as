/* a simple data structure: mutable, singly linked list */
type List<T> = {head: T; var tail: List<T>}?;

type subscription = {
  post : shared Text -> async ();
  cancel : shared () -> ();
};

type IClient = actor {
   send: shared Text -> async ();
};

type IServer = actor {
  subscribe: IClient -> async subscription;
};

actor Server = {
   private var nextId:Nat= 0;
   private var log:List<Text> = null;
   private var clients:List<(Nat,IClient)> = null;
   private broadcast(id:Nat,message:Text) : async () {
      var next = clients;
      loop {
         switch (next) {
      	    case null return;
	    case (n?) {
	        if ( n.head.0 != id) {
		  await (n.head.1).send(message);
		};
		next := n.tail;
              };
           };
        };
     };

   subscribe(client:IClient) : async subscription {
     let id = nextId;
     nextId += 1;
     let cs = new { head = (id,client); var tail = clients};
     clients := cs?;
     return (new {
       post =  (shared func (message:Text) : async () { await broadcast(id,message);});
       cancel = (shared func () {unsubscribe(id);});
     });
   };

   private unsubscribe(id:Nat) {
      var prev:List<(Nat,IClient)> = null;
      var next = clients;
      loop {
         switch (next) {
      	    case null return;
	    case (n?) {
	    	if ((n.head.0) == id)
		  { switch (prev) {
		      case null clients := n.tail;
		      case (p?) p.tail := n.tail;
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
   private var name : Text = "";
   private var server: IServer?  = null;
   go (n:Text,s:IServer) : async () {
       name := n;
       server := s?;
       let sub = await s.subscribe(this);
       await sub.post("hello from " # name);
       await sub.post("goodbye from " # name);
       sub.cancel();
   };
   send(msg:Text) : async () {
      print name; print " received "; print msg; print "\n";
   };
};



let bob = Client();
let alice = Client();
let charlie = Client();
let _ = bob.go("bob",Server);
let _ = alice.go("alice",Server);
let _ = charlie.go("charlie",Server);


/* features to add:
   don't broadcast to sender
   subscribe should also return unsubscribe capability (use actor identity (if we support that) or add a tag)
   Client's 'do not disturb' that 'revokes' send by changing its behaviour.
*/   

/* runs fine in interpreter, crashes compiler
   bugs observed:
   design flaws:
     - it's annoying you can't await in a ()-message, just an awaitable one (eg. Client.go()).
     - we can't (synchronously) subscribe in the Client constructor as its async, need a separate 'go' method.
   tc: reordering IServer and IClient leads to a complaint that subscribe has non-sharable argument	  type - we probably need to normalize while checking.
   compiler:
        - parameterising Client on s:IServer argument complains about non-closed actor (possible I had a typo though)
*/
   
