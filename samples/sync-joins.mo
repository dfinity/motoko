/* a simple implementation of join patterns in AS 

loosely based on:

https://www.microsoft.com/en-us/research/wp-content/uploads/2007/01/The-Joins-Concurrency-Library.pdf

This version extends async-joins by allowing at most one pseudo-synchronous request per pattern,
a la Polyphonic C#.

Brings up loads of isses:

- Its awkward that we cannot abstract over sharable types, and that async T is illegal when T is a type parameter. This means we have to mono-morphise the API for different request return types,
  which sucks. I can't yet see a work-around.

- There is no efficient way to logically block a request, it has to yield and poll the resource in a  loop.
  Essentially this is because we cannot park response continuations in data-structures for another call-tree to resume (i.e. wait queues), just like you can't implemented threading in a user library if you don't have  continuations as values.

- we need to additional trips through the scheduler every time we want to use a local function that sends because its body must have async type (and be manifestly async, though this could be dropped). A better effect system would allow us to abstract sends (and awaits) out better.

*/


import Prim "mo:prim";

let print = Prim.debugPrint;

// TODO: make addClause, scan, match and get private by passing them directly to Msg, Conj (in place of Join argument) etc.

//  for awaitable channels (that return results) see awaitablejoins.as

// TODO: use tuple
type List<T> = ?{head: T; tail: List<T>};

func rev<T>(l:List<T>) : List<T> {
    func go<T>(l:List<T>, acc:List<T>) : List<T> {
	switch l {
	case (null) acc;
	case (?l) go<T>(l.tail, ? { head = l.head; tail = acc });
	};
    };
    go<T>(l,null);
};

// mutable message queue
class Queue<T>() {
    private var front : List<T> = null;
    private var back : List<T> = null;

    public func isEmpty() : Bool {
	switch(front,back) {
	case (null,null) true;
	case _ false;
	};
    };

    public func enqueue(t : T) {
	back := ? { head = t; tail = back };
    };

    public func dequeue() : T {
	switch (front,back)
	{
	    case (null, null) { assert false;loop{};};
	    case (?f, _) {
		let t = f.head;
		front := f.tail;
		t;
	    };
	    case (null,?b) {
      		front := rev<T>(back);
		back := null;
		dequeue();
	    };
	};
    };
};


type Result<R> = { #ok : R; #err : Error };

type Item<A,R> = { arg:A; var reply: ?Result<R> };

class Req<A,R> (join : Join) {
    let queue : Queue<Item<A,R>> = Queue<Item<A,R>>();

    public func isEmpty():Bool = queue.isEmpty();

    // would like to return async R but cann't because R not sharable - workaround by
    // by returning a pair of functions for the client, where R is known, to use instead (yuck).
    // see boilerplater code below around calls to send.
    public func send(a : A) : (() -> async (), () -> ?Result<R>) {
        let item = {arg = a; var reply : ?Result<R> = null};
	(func () : async () {
	  queue.enqueue(item);
	  switch (join.scan()) {
  	   case (?clause) (ignore clause.fire()());
	   case _ ();
          }
	 },
	func () { return item.reply });
    };

    public func match():Bool {
      not(queue.isEmpty())
    };

    public func get():(A, Result<R> -> ()) {
      let item = queue.dequeue();
      (item.arg, func r { item.reply := ?r })
    };
};


class Msg<A> (join : Join) {
    let queue : Queue<A> = Queue<A>();

    public func isEmpty():Bool = queue.isEmpty();

    public func post(a : A) : async () {
	queue.enqueue(a);
	switch (join.scan()) {
  	  case (?clause) (ignore clause.fire()());
          case null return;
        };
    };

    public func match():Bool {
      not(queue.isEmpty())
    };

    public func get():A { queue.dequeue();};
};

type Pat<A> = {
    match : () -> Bool;
    get : () -> A;
};


class And<A,B>(pat:Pat<A>, chan:Msg<B>) : Pat<(A,B)> {
    public func match():Bool { (pat.match() and (not (chan.isEmpty()))); };
    public func get():((A,B)) { (pat.get(), chan.get());}
};


class Join() = this {
    private var patterns : List<AbsClause> = null;

    private func addClause(c:AbsClause) {
	patterns := ? { head = c; tail = patterns };
    };

    public func scan() : ? AbsClause {
	var next = patterns;
	loop {
            switch (next) {
            case null return null;
            case (?n) {
                if (n.head.match()) {
                    return ? n.head;
                };
                next := n.tail;
            };
            };
	};
    };

    public func Message<A>() : Msg<A> = Msg<A>(this);
    public func Request<A,R>() : Req<A,R> = Req<A,R>(this);

    public func When<A>(pat : Pat<A>, cont: A-> async ()) {
      addClause(Clause<A>(pat,cont));
    };

    // would like to parameterize by R but can't due to sharability restriction of async R
    public func WhenUnit<A,B/*,R*/>(req: Req<A,/*R*/()>, p: Pat<B>, cont: (A,B) -> async /*R*/()) {
        addClause(UnitClause<A,B>(req, p, cont));
    };

    public func WhenText<A,B/*,R*/>(req: Req<A,/*R*/Text>, p: Pat<B>, cont: (A,B) -> async /*R*/Text) {
        addClause(TextClause<A,B>(req, p, cont));
    };


};

type AbsClause = {
    match: () -> Bool;
    fire: () -> (() -> async ());
};

func Clause<A>(pat: Pat<A>, cont: A-> async ()) : AbsClause = object {
    public func match(): Bool { pat.match(); };
    public func fire() : () -> async () {
        let a = pat.get();
        func () : async () { ignore cont(a); } // optimize me to avoid scheduling
    };
};

// Again, would like to parameterize by R but can't due to sharability restriction of async R
func UnitClause<A, B /* R */ >(req: Req<A,/* R */()>,
                              pat: Pat<B>,
                              cont: (A,B)-> async /*R*/ ()): AbsClause = object {
    public func match(): Bool { req.match() and pat.match(); };
    public func fire() : () -> async () { // redundant scheduling
        let (a,r) = req.get();
        let b = pat.get();
        func () : async () { // redundant scheduling
	    r(try (#ok (await cont(a,b)))
              catch e { #err e });
	}
    }
};


func TextClause<A, B, /* R */ >(req: Req<A,Text>,
                              pat: Pat<B>,
                              cont: (A,B)-> async /*R*/ Text): AbsClause = object {
    public func match(): Bool { req.match() and pat.match(); };
    public func fire() : () -> async () {
        let (a,r) = req.get();
        let b = pat.get();
        func () : async () {
	    r(try (#ok (await cont(a,b)))
              catch e { #err e });
	}
    }
};

// Examples

// an unbounded, asynchronous buffer
actor Buffer = {
    let j  = Join();
    let put = j.Message<Text>();
    let get = j.Request<(),Text>();
    j.WhenText<(),Text>(
      get,
      put,
      // type argument inference, please
      // inference for scoped functions too, please, so we can omit the annotation
      func (_:(),t:Text) : async Text {
	t
     }
    );
    public func Put(t : Text) {	ignore put.post(t); };
    public func Get() : async Text {
        // boilerplate, should be in send, but can't due to no sharability abstraction.
        let (send,receive) = get.send(());
        await send();
        loop {
            switch (receive()) {
            case (? (#ok r))  { return r };
            case (? (#err e)) { throw e };
            case null { await async () };
            }
        }
    }
};


ignore async {
  Buffer.Put("Hello\n");
  Buffer.Put("World\n");
  print (await Buffer.Get());
  print (await Buffer.Get());
};


// a synchronous lock
actor Lock = {
    let j  = Join();
    let free = j.Message<()>();
    let acquire = j.Request<(),()>();

    public func Release() { ignore free.post(());};

    j.WhenUnit<(),()>(
        acquire,free,
	func (_:(),_:()) : async ()  {  // type annotation inference, please
	}
    );


    public func Acquire() : async () {
        // boilerplate, should be in send,
        // but can't be due to no sharability nor await abstraction.
        let (send,receive) = acquire.send(());
        await send();
        loop {
            switch (receive()) {
            case (? (#ok r))  { return r };
            case (? (#err e)) { throw e };
            case null { await async () };
            }
        }
    };

    public func Init() { ignore free.post(()); }; // lock initially free

};


// a shared output buffer, protected by Lock
actor Resource {
    var buffer = "";
    public func write(t:Text) { buffer := buffer # t;};
    public func get() : async Text { buffer; };
};

actor Alice {
    let name =  ["a","l","i","c","e"];

    public func send() :  () {
        await Lock.Acquire();
    	for (s in name.vals())
	{
            Resource.write(s);
            await (async ()); // yield to allow interleavings
	};
	print (await Resource.get());
	print ("\n");
	Lock.Release();
    };

    public func start() { send();}
};

// just like Alice
actor Charlie {
    let name =  ["c","h","a","r","l","i","e"];

    public func send() : () {
        await Lock.Acquire();
        for (s in name.vals())
	{
            Resource.write(s);
            await (async ()); // yield to allow interleavings
	};
	print (await Resource.get());
	print ("\n");
	Lock.Release();
    };

    public func start() { send();}
};

// proper locking ensures the writes to Resource aren't interleaved.
Lock.Init();
Alice.start();
Charlie.start();
