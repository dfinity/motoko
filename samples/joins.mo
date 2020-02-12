/* a simple implementation of join patterns in AS 

loosely based on:

https://www.microsoft.com/en-us/research/wp-content/uploads/2007/01/The-Joins-Concurrency-Library.pdf

*/

// TODO: make addClause, scan, match and get private by passing them directly to Atom, Conj (in place of Join argument) etc.

//  for awaitable channels (that return results) see awaitablejoins.as

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

    public func isEmpty() : Bool {
	switch(front,back) {
	case (null,null) true;
	case _ false;
	};
    };

    public func enqueue(t : T) {
	back := ? (new { head = t; tail = back });
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

class Chan<A> (join : Join) {
    let queue : Queue<A> = Queue<A>();

    public func isEmpty():Bool = queue.isEmpty();

    public func post(a : A) {
	queue.enqueue(a);
	join.scan();
    };

    public func dequeue() : A
    {
	queue.dequeue();
    }
};

type Pat<A> = {
    And : <B> Chan<B> -> Pat<(A,B)>; // causes type-checker looping without hack!
    Do : (A -> ()) -> ();
    match : () -> Bool;
    get : () -> A;
};

class Atom<A>(join : Join, chan : Chan<A>) : Pat<A> = this {
    public func And<B>(chan : Chan<B>):Pat<(A,B)> = Conj<A,B>(join, this, chan);
    public func Do (cont: A-> ()) { join.addClause(Clause<A>(this, cont));};

    public func match():Bool { not(chan.isEmpty()); };
    public func get():A { chan.dequeue();}
};

class Conj<A,B>(join : Join, pat:Pat<A>, chan:Chan<B>) : Pat<(A,B)> = this  {
    public func And<C>(chan : Chan<C>):Pat<((A,B),C)> =
     	Conj<(A,B),C>(join, this, chan);
    public func Do (cont: ((A,B)) -> ()) { join.addClause(Clause<(A,B)>(this, cont));};

    public func match():Bool { (pat.match() and (not (chan.isEmpty()))); };
    public func get():((A,B)) { (pat.get(), chan.dequeue());}
};


class Join() = this {
    private var patterns : List<AbsClause> = null;

    addClause(c:AbsClause) {
	patterns := ?(new { head = c; tail = patterns });
    };

    public func scan() {
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

    public func Create<A>() : Chan<A> = Chan<A>(this);

    public func When<A>(c:Chan<A>):Pat<A> = Atom<A>(this, c);
};

type AbsClause = {
    match: () -> Bool;
    fire: () -> ();
};

func Clause<A>(pat: Pat<A>, cont: A->()) : AbsClause = new {
    func match():Bool { pat.match(); };
    func fire() {
	cont(pat.get());
    };
};


// Examples

// an unbounded, asynchronous buffer
actor Buffer = {
    let j  = Join();
    let put = j.Create<Text>();
    let get = j.Create<shared Text-> ()>();
    j.When<Text>(put).And<shared Text-> ()>(get). // type argument inference, please
	Do( func ( (t,c) : (Text, (shared Text -> ())))  {  // type annotation inference, please
		c(t);
	}
    );
    public func Put(t : Text) {	put.post(t); };
    public func Get(c : shared Text-> ()) { get.post(c); }
};


Buffer.Put("Hello\n");
Buffer.Put("World\n");
Buffer.Get(shared func(t:Text) { print(t);});
Buffer.Get(shared func(t:Text) { print(t);});

// an asynchronous lock
type Release = shared () -> ();
actor Lock = {
    let j  = Join();
    let free = j.Create<()>();
    let acquire = j.Create<shared Release -> async ()>();
    func release() { free.post(());};
    j.When<shared Release-> async ()>(acquire).And<()>(free).
	    Do( func ( (k,_) : (shared Release -> async (), ()))  {  // type annotation inference, please
		    let _ = k(shared func () = release());
	    }
    );
    release(); // lock initially free
    public func Acquire(k: shared Release -> async ()) { acquire.post(k); }
};

// a shared output buffer, protected by Lock
actor Resource {
    var buffer = "";
    public func write(t:Text) { buffer := buffer # t;};
    public func get() : async Text { buffer; };
};

actor Alice {
    let name =  ["a","l","i","c","e"];

    public func send(release:Release) : async () {
    	for (s in name.vals())
	{
            Resource.write(s);
            await (async ()); // yield to allow interleavings
	};
	print (await Resource.get());
	print ("\n");
	release();
    };

    public func start() { Lock.Acquire(send);}
};

// just like Alice
actor Charlie {
    let name =  ["c","h","a","r","l","i","e"];

    public func send(release:Release) : async () {
        for (s in name.vals())
	{
            Resource.write(s);
            await (async ()); // yield to allow interleavings
	};
	print (await Resource.get());
	print ("\n");
	release();
    };

    public func start() { Lock.Acquire(send);}
};

// proper locking ensures the writes to Resource aren't interleaved.
Alice.start();
Charlie.start();
