/* a simple implementation of join patterns in AS 

loosely based on:

https://www.microsoft.com/en-us/research/wp-content/uploads/2007/01/The-Joins-Concurrency-Library.pdf

run with
../src/moc -r --package stdlib ../stdlib/src async-joins.mo


*/

import Nat "mo:stdlib/nat";
import Prim "mo:prim";

let print = Prim.debugPrint;

// TODO: make addClause, scan, match and get private by passing them directly to Chan, Conj (in place of Join argument) etc.

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
        switch (front, back) {
            case (null, null) true;
            case _ false;
        };  
    };

    public func enqueue(t : T) {
        back := ? { head = t; tail = back };
    };

    public func dequeue() : T {
        switch (front,back) {
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


class And<A,B>(pat:Pat<A>, chan:Chan<B>) : Pat<(A,B)> {
    public func match():Bool { 
        pat.match() and (not (chan.isEmpty()));
    };
    public func get():((A,B)) { 
        (pat.get(), chan.get());
    }
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

    public func Create<A>() : Chan<A> = Chan<A>(this);

    public func When<A>(pat : Pat<A>, cont: A-> async ()) {
      addClause(Clause<A>(pat,cont));
    };
};

type AbsClause = {
    match: () -> Bool;
    fire: () -> (() -> async ());
};

func Clause<A>(pat: Pat<A>, cont: A-> async ()) : AbsClause = object {
    public func match():Bool { pat.match(); };
    public func fire() : () -> async () {
        let a = pat.get();
        func () : async () { ignore cont(a); } // optimize me to avoid scheduling
    };
};


// Examples

// an unbounded, asynchronous buffer
actor Buffer = {
    let j  = Join();
    let put : Chan<Text> = j.Create();
    let get : Chan<shared Text-> ()> = j.Create();
    j.When(
      And(put, get),
      // inference for scoped functions too, please, so we can omit the annotation
      func ((t:Text,c:shared Text -> ())) : async () {
          c(t);
     }
    );
    public func Put(t : Text) {	ignore put.post(t); };
    public func Get(c : shared Text-> ()) { ignore get.post(c); }
};


ignore async {
  Buffer.Put("Hello\n");
  Buffer.Put("World\n");
  Buffer.Get(shared func(t:Text) { print(t);});
  Buffer.Get(shared func(t:Text) { print(t);});
};


// an asynchronous lock
type Release = shared () -> ();
actor Lock = {
    let j  = Join();
    let free : Chan<()> = j.Create();
    let acquire : Chan<shared Release -> ()> = j.Create();

    public func Release() { ignore free.post(());};

    j.When(
        And(acquire,free),
        func ((k:shared Release -> (), ())) : async ()  {  // type annotation inference, please
            k(Release);
        }
    );


    public func Acquire(k: shared Release -> ()) { ignore acquire.post(k); };

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

    public func send(release:Release) :  () {
    for (s in name.vals())
    {
        Resource.write(s);
        await (async ()); // yield to allow interleavings
    };
    print (await Resource.get());
    print ("\n");
    release();
    };

    public func start() { ignore Lock.Acquire(send);}
};

// just like Alice
actor Charlie {
    let name =  ["c","h","a","r","l","i","e"];

    public func send(release:Release) : () {
    for (s in name.vals())
    {
        Resource.write(s);
        await (async ()); // yield to allow interleavings
    };
    print (await Resource.get());
    print ("\n");
    release();
    };

    public func start() { ignore Lock.Acquire(send);}
};

// proper locking ensures the writes to Resource aren't interleaved.
Lock.Init();
Alice.start();
Charlie.start();