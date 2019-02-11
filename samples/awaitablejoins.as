/* a simple implementation of join patterns in AS 

loosely based on:

https://www.microsoft.com/en-us/research/wp-content/uploads/2007/01/The-Joins-Concurrency-Library.pdf

*/

// TODO: make addClause, scan, match and get private by passing them directly to Atom, Conj (in place of Join argument) etc.

// TODO: add support for awaitable channels (that return results)

type List<T> = ?{head: T; tail: List<T>};

func rev<T>(l:List<T>) : List<T> {
    func go<T>(l:List<T>, acc:List<T>) : List<T> {
	switch l {
	case (null) acc;
	case (?l) go<T>(l.tail, ? (new { head = l.head; tail = acc }));
	};
    };
    go<T>(l,null);
};

// mutable message queue
class Queue<T>() {
    private var front : List<T> = null;
    private var back : List<T> = null;

    isEmpty() : Bool {
	switch(front,back) {
	case (null,null) true;
	case _ false;
	};
    };

    enqueue(t : T) {
	back := ? (new { head = t; tail = back });
    };
    dequeue() : T {
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

class Chan<A,R> (join : Join) = {
    private queue : Queue<A> = Queue<A>();

    isEmpty():Bool = queue.isEmpty();

    post(a : A) {
	queue.enqueue(a);
	join.scan();
    };
    dequeue() : A
    {
	queue.dequeue();
    }
};

type Pat<A,R> = {
    And : <B> Chan<B,R> -> Pat<(A,B),R>; // causes type-checker looping without hack!
    Do : (A -> R) -> ();
    match : () -> Bool;
    get : () -> A;
};

func Atom<A,R>(join : Join, chan : Chan<A,R>) : Pat<A,R> = new this {
    And<B>(chan : Chan<B,R>):Pat<(A,B),R> = Conj<A,B,R>(join, this, chan);
    Do (cont: A-> R) { join.addClause(Clause<A,R>(this, cont));};

    match():Bool { not(chan.isEmpty()); };
    get():A { chan.dequeue();}
};

func Conj<A,B,R>(join : Join, pat:Pat<A,R>, chan:Chan<B,R>) : Pat<(A,B),R> = new this {
    And<C>(chan : Chan<C,R>):Pat<((A,B),C),R> =
     	Conj<(A,B),C,R>(join, this, chan);
    Do (cont: ((A,B)) -> R) { join.addClause(Clause<(A,B),R>(this, cont));};

    match():Bool { (pat.match() and (not (chan.isEmpty()))); };
    get():((A,B)) { (pat.get(), chan.dequeue());}
};


class Join() = this {
    private var patterns : List<AbsClause> = null;

    addClause(c:AbsClause) {
	patterns := ?(new { head = c; tail = patterns });
    };

    scan() {
	var next = patterns;
	loop {
            switch (next) {
            case null return;
            case (?n) {
                if (n.head.match()) {
                    return n.head.fire();
                };
                next := n.tail;
            };
            };
	};
    };

    Create<A>() : Chan<A,Any> = Chan<A,Any>(this);
    Request<A,C>() : Chan<A,C> = Chan<A,C>(this);

    When<A>(c:Chan<A,Any>):Pat<A,Any> = Atom<A,Any>(this, c);
};

type AbsClause = {
    match: () -> Bool;
    fire: () -> ();
};

func Clause<A,R>(pat: Pat<A,R>, cont: A->R) : AbsClause = new {
    match():Bool { pat.match(); };
    fire() : () {
//	cont(pat.get());
    };
};


// Examples

// an unbounded, asynchronous buffer
actor Buffer = {
    private j  = Join();
    private put = j.Create<Text>();
    private get = j.Create<shared Text-> ()>();
    private init  =
	j.When<Text>(put).And<shared Text-> ()>(get). // type argument inference, please
	Do( func ( (t,c) : (Text, (shared Text -> ()))) :(())  {  // type annotation inference, please
		c(t);
	}
	  );
    Put(t : Text)
    {	put.post(t);
    };
    Get(c : shared Text-> ()) = get.post(c);
};


Buffer.Put("Hello\n");
Buffer.Put("World\n");
Buffer.Get(shared func(t:Text) { print(t);});
Buffer.Get(shared func(t:Text) { print(t);});

// an asynchronous lock
type Release = shared () -> ();
actor Lock = {
    private j  = Join();
    private free = j.Create<()>();
    private acquire = j.Create<shared Release -> async ()>();
    private release() = free.post(());
    private init  = {
	j.When<shared Release-> async ()>(acquire).And<()>(free).
	    Do( func ( (k,_) : (shared Release -> async (), ())) :(()) {  // type annotation inference, please
		    let _ = k(shared func () = release());
	    }
	      );
	release(); // lock initially free
    };

    Acquire(k: shared Release -> async ()) { acquire.post(k); }
};

// a shared output buffer, protected by Lock
actor Resource {
    private var buffer = "";
    write(t:Text) { buffer := buffer # t;};
    get():async Text{ buffer; };
};

actor Alice {
    private name =  ["a","l","i","c","e"];

    send(release:Release) : async () {
    	for (s in name.vals())
	{
            Resource.write(s);
            await (async ()); // yield to allow interleavings
	};
	print (await Resource.get());
	print ("\n");
	release();
    };

    start() { Lock.Acquire(send);}
};

// just like Alice
actor Charlie {
    private name =  ["c","h","a","r","l","i","e"];

    send(release:Release) : async () {
        for (s in name.vals())
	{
            Resource.write(s);
            await (async ()); // yield to allow interleavings
	};
	print (await Resource.get());
	print ("\n");
	release();
    };

    start() { Lock.Acquire(send);}
};

// proper locking ensures the writes to Resource aren't interleaved.
Alice.start();
Charlie.start();
