

// mini parsec implementation based on opal

// compile with 
// ../src/moc --package stdlib ../stdlib/src parsec.mo
 
import Iter "mo:stdlib/iter";
import List "mo:stdlib/list";
import Char "mo:stdlib/char";
import Text "mo:stdlib/text";
import Prim "mo:prim";

module Parsec {

module Lazy {
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

module LazyStream = {
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

func o<T,U,V>(f:T->U,g:U -> V) : T -> V = func x = g(f(x));

// func parse(parser:T,input)
// prims


}