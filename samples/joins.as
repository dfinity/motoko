type List<T> = ?{head: T; var tail: List<T>};

func rev<T>(l:List<T>, acc:List<T>) : List<T> {
    switch l {
      case (null) acc;
      case (?l) rev<T>(l.tail, ? (new { head = l.head; var tail = acc }));
    };
};

class Queue<T>() {
  private var front : List<T> = null;
  private var back : List<T> = null;

  isEmpty() : Bool {
    switch(front,back) {
     case (null,null) true;
     case _ false;
    };
  };

  enqueue(t : T) = {
    switch(front,back)
    {
      case (null, null)
      { front := ? (new { head = t; var tail = null });
      };
      case (_, ?l) {
        back := ? (new { head = t; var tail = back });
      };
    };
  };
  dequeue():T = {
    switch (front,back)
    {
      case (null, null) { assert false; dequeue()};
      case (_, ?l) {
         let t = l.head;
	 back := l.tail;
	 t;
      };
      case (?l, null) {
      	 back := rev<T>(front,null);
	 front := null;
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
     join : Join;
//     And : <B> Chan<B> -> Pat<(A,B)>; // causes type-checker looping!
     Do : (A -> ()) -> ();
     match : () -> Bool;
     get : () -> A;
};

//class Atom<A <: Shared>(c : Chan<A>) = this {
func Atom<A>(c : Chan<A>) : Pat<A> = new this {
     join : Join = c.join;
     chan : Chan<A> = c;

     And<B>(c : Chan<B>):Pat<(A,B)> =
          Conj<A,B>(this, c);

     Do (k: A-> ()) {};

     match():Bool { not(chan.isEmpty()); };

     get():A { chan.dequeue();}
};

//class Conj<A <: Shared, B <: Shared>(p:Pat<A>,c:Chan<B>) = this {
func Conj<A,B>(p:Pat<A>, c:Chan<B>) : Pat<(A,B)> = new this {
     join : Join = c.join;
     pat : Pat<A> = p;
     chan : Chan<B> = c;
     And<C>(c : Chan<C>):Pat<((A,B),C)> =
     	 Conj<(A,B),C>(this,c);
     Do (k: ((A,B)) -> ()) {};
     match():Bool { (pat.match() and (not (chan.isEmpty()))); };
     get():((A,B)) { (pat.get(), chan.dequeue());}
};


class Join() = this {
  private var count : Int = 0;
  private var patterns : List<AbsClause> = null;

  addClause(c:AbsClause) {
    patterns := ?(new { head = c; var tail = patterns });
  };

  scan() { };

  Create<A>() : Chan<A> = {
    count += 1;
    Chan<A>(this, count);
  };

  When<A>(c:Chan<A>):Pat<A> = Atom<A>(c);
};

type AbsClause = {
  match: () -> Bool;
  fire: () -> ();
};

//class Clause<A <: Shared >(pat: Pat<A>, cont: A->()) = {
func Clause<A>(pat: Pat<A>, cont: A->()) : AbsClause = new {
   match():Bool { pat.match(); };
   fire() {
	 cont(pat.get());
   };
};

class Buffer() {
    private j : Join = Join();
    private put : Chan<Int> = j.Create<Int>();
    private get : Chan<shared Int -> ()> = j.Create<shared Int-> ()>();
    private dummy : () =
      j.When<Int>(put).And<shared Int-> ()>(get). // type argument inference, please
      Do( func ( (i,f) : (Int, (shared Int -> ())))  {  // type annotation inference, please
	    f(i);
	  }
      );
    Put(i : Int) = put.post(i);
    Get(f : shared Int-> ()) = get.post(f);
};





