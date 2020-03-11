

// mini parsec implementation based on opal

// compile with 
// ../src/moc --package stdlib ../stdlib/src parsec.mo
 
import Iter "mo:stdlib/iter";
import List "mo:stdlib/list";
import Char "mo:stdlib/char";
import Text "mo:stdlib/text";
import Prim "mo:prim";

module Parsec {

public module Lazy {
  public class t<A>(f:() -> A) {
    var state : {#delay : (() -> A); #result : A} 
        = #delay f;
    public func force(): A {
        switch state {
          case (#delay f) { let r = f(); state := #result r; return r; };
          case (#result r) { return r; };
        }
      };
  };
};

public module LazyStream = {
  public type t<A> = ? (A, Lazy.t<t<A>>);

  public func ofIter<A>(iter:Iter.Iter<A>) : t<A> {
   func next(iter:Iter.Iter<A>) : t<A> {
      switch (iter.next()) {
        case (?a) (?(a, Lazy.t(func () : t<A> { next iter ; })));
        case null null
      };
    };
   next iter
  };

  public func ofFunc<A>(f:() -> ?A) : t<A> {
    func next(f:()->?A) : t<A> {
      switch (f ()) {
       case (?a) (?(a, Lazy.t(func () : t<A> { next f; })));
       case null null
      };
    };
    next f
  };

  public func ofText(t : Text):t<Char> = ofIter(Text.toIter t);
};

// utils

func implode(cs : List.List<Char>) : Text = 
  List.foldRight(cs, "", func(c:Char,t:Text):Text { Prim.charToText c # t}); // TBR

func explode(t : Text) : List.List<Char> { 
   var l : List.List<Char> = null;
   for (c in t.chars()){
      l := List.push(c,l);
   };
   List.rev(l);
};

func o<T,U,V>(f:T->U,g:U -> V) : T -> V { func x { g(f(x)) }} ;

public type Input<Token> = LazyStream.t<Token>;
public type Monad<Token, Result> = ?(Result, Input<Token>);
public type Parser<Token, Result> = Input<Token> -> Monad<Token,Result>;

public func ret<Token,A>(x:A):Parser<Token,A> { func input { ?(x,input) }};

public func bind<Token,A,B>(pa: Parser<Token,A>, f: A -> Parser<Token,B>): Parser<Token,B> {
    func input { 
      switch (pa input) {
        case (?(result,input)) (f result input);
        case null null;
      }
   } 
};

public func choose<Token,A>(x: Parser<Token,A>):Parser<Token,A> -> Parser<Token,A> {
  func y { 
    func input { 
      let ret = x input;
      switch ret {
        case (? _) ret;
        case null (y input);
      }
   }
  } 
};

public func mzero<Token,A>() : Parser<Token,A> = func (_ : Input<Token>) : Monad<Token,A> =  null;

public func any<Token>(ls : Input<Token>):Monad<Token,Token> {
  switch ls {
    case (?(token, input)) (?(token, input.force()));
    case null null;
  }
};

public func statisfy<Token>(test : Token -> Bool) : Parser<Token,Token> {
   bind((func ls = any ls) // needs implicit instantiation
        : Parser<Token,Token>,
        (func res = if (test res) (ret res) else mzero())
        : Token -> Parser<Token,Token>)
        
};

public func eof<Token,A>(x : A) : Parser<Token,A> {
  func input { 
    switch input {
      case null (?(x,null));
      case _ null;
    }
  }
};


// derived



// =>
public func then<Token,A,B>(pa:Parser<Token,A>,f: A -> B) : Parser<Token,B> {
  bind(pa,
      (func a = ret (f a)) : A -> Parser<Token,B>);
};

// <<
public func left<Token,A,B>(pa:Parser<Token,A>,pb: Parser<Token,B>) : Parser<Token,A> {
  bind(pa,
      (func a = bind(pb,(func _ = ret a) : B -> Parser<Token,A>))
      : A -> Parser<Token,A>);
};

// <~>
public func cons<Token,A>(pa:Parser<Token,A>,pas: Parser<Token,List.List<A>>) : Parser<Token,List.List<A>> {
  bind(pa,
      (func a = bind(pas,(func as = ret (List.push(a,as))) : List.List<A> -> Parser<Token,List.List<A>>))
      : A -> Parser<Token,List.List<A>>);
};


public func choice<Token,A>(ps: List.List<Parser<Token,A>>) : Parser<Token,A> {
  switch ps {
    case null mzero();
    case (?(p,ps)) (choose p (choice ps));
  };
};

public func count<Token,A>(n:Nat,pa:Parser<Token,A>) : Parser<Token,List.List<A>> {
  if (n > 0) cons(pa,count(n-1,pa)) 
  else ret (List.nil<A>()); // needs <A> or constraint.
};

public func between<Token,A,B,C>(pa : Parser<Token,A>,pb: Parser<Token,B>, pc : Parser<Token,C>> {
    righ(op,left(pc,pb));
}


}