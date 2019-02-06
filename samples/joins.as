class Chan<A <: Shared> ( j : Join, i : Int ) = {
  join : Join = j;
  id : Int = i;
  post(a : A){};
};

type Pat<A <: Shared> = {
     join : Join;
//     And : <B <: Shared> Chan<B> -> Pat<(A,B)>; // causes looping!
     Do : (A -> ()) -> ();
};

class Atom<A <: Shared>(c : Chan<A>) = this {
     join : Join = c.join;
     chan : Chan<A> = c;
     And<B <: Shared>(c : Chan<B>):Pat<(A,B)> =
          Conj<A,B>(this,c); 
     Do (k: A-> ()) {};
};

class Conj<A <: Shared,B <: Shared>(p:Pat<A>,c:Chan<B>) = this {
     join : Join = c.join;
     pat : Pat<A> = p;
     chan : Chan<B> = c;
     And<C <: Shared>(c : Chan<C>):Pat<((A,B),C)> =
     	 Conj<(A,B),C>(this,c);
     Do (k: ((A,B)) -> ()) {};
};


class Join() = this {
  private var count : Int = 0;
  Create<A <: Shared>() : Chan<A> = {
    count += 1;
    Chan<A>(this, count);
  };

  When<A <: Shared>(c:Chan<A>):Atom<A> = Atom<A>(c);
};
