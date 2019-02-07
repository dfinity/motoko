// TODO: make addClause private by passing it directly to Atom, Conj (in place of Join argument)
// TODO: remove redundant types on fields
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

class Chan<A> ( j : Join, i : Int ) = {
  join : Join = j;
  id : Int = i;
  queue : Queue<A> = Queue<A>();

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

type Pat<A> = {
//     And : <B> Chan<B> -> Pat<(A,B)>; // causes type-checker looping!
//     And : <B> Chan<B> -> Conj<A,B>; // causes type-checker looping!
     Do : (A -> ()) -> ();
     match : () -> Bool;
     get : () -> A;
};

//class Atom<A <: Shared>(c : Chan<A>) = this {
type Atom<A> =
{
     And : <B>Chan<B> -> Conj<A,B>;
     Do : (A -> ()) -> ();
     match : () -> Bool;
     get : () -> A;
};

type Conj<A,B> =
{
//     And : <C>Chan<C> -> Conj<(A,B),C>; // loops typechecker!
     Do : (((A,B)) -> ()) -> ();
     match : () -> Bool;
     get : () -> ((A,B));
};

func Atom<A>(join : Join, chan : Chan<A>) : Atom<A> = new this {
     And<B>(chan : Chan<B>):Conj<A,B> = Conj<A,B>(join, this, chan);
     Do (cont: A-> ()) { join.addClause(Clause<A>(this, cont));};

     match():Bool { not(chan.isEmpty()); };
     get():A { chan.dequeue();}
};

//class Conj<A <: Shared, B <: Shared>(p:Pat<A>,c:Chan<B>) = this {
func Conj<A,B>(join : Join, pat:Pat<A>, chan:Chan<B>) : Conj<A,B> = new this {
     And<C>(chan : Chan<C>):Conj<(A,B),C> =
     	 Conj<(A,B),C>(join, this, chan);
     Do (cont: ((A,B)) -> ()) { join.addClause(Clause<(A,B)>(this, cont));};

     match():Bool { (pat.match() and (not (chan.isEmpty()))); };
     get():((A,B)) { (pat.get(), chan.dequeue());}
};


class Join() = this {
  private var count : Int = 0; /* TBD */
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

  Create<A>() : Chan<A> = {
    count += 1;
    Chan<A>(this, count);
  };

  When<A>(c:Chan<A>):Atom<A> = Atom<A>(this, c);
};

type AbsClause = {
  match: () -> Bool;
  fire: () -> ();
};

//class Clause<A <: Shared >(pat: Pat<A>, cont: A->()) = {
func Clause<A>(pat: Pat<A>, cont: A->()) : AbsClause = new {
   match():Bool { pat.match(); };
   fire() {
   	 print ("fire");
	 cont(pat.get());
   };
};

// an example

actor Buffer = {
    private j /* : Join */ = Join();
    private put /* : Chan<Text> */ = j.Create<Text>();
    private get /* : Chan<shared Text -> ()>  */ = j.Create<shared Text-> ()>();
    private dummy /* : () */ =
      j.When<Text>(put).And<shared Text-> ()>(get). // type argument inference, please
      Do( func ( (t,c) : (Text, (shared Text -> ())))  {  // type annotation inference, please
	    c(t);
	  }
      );
    Put(t : Text) 
    {	print ("Putting" # t);
	put.post(t);
    };
    Get(c : shared Text-> ()) = get.post(c);
};


Buffer.Put("Hello");
Buffer.Put("World");
Buffer.Get(shared func(t:Text) { print(t);});
Buffer.Get(shared func(t:Text) { print(t);});



