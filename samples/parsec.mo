// mini parsec implementation based on OCaml opal
//  https://github.com/pyrocat101/opal/
// implemented to exercise motoko argument type inference

// Some deviations due to lack of polymorphic comparison, meaning we need to pass
// eq and leq functions to the singleton combinators (see below)
// We typically use tuples (e.g. bind), not currying (eg. Opal's choose),
// but could uniformly refactor to either as in Ocaml.

// not terrible, but see comments and remaining explicit instantiations

// Lesson: we often have to choose between argument type inference and
// unnotated anonymous function arguments. Why? type argument nference relies on
// inferring argument types but implicit function typing relies on
// checking the function against an expect argument type.
// Choose your poison.

// compile with
// ../src/moc --package stdlib ../stdlib/src parsec.mo

import Array "mo:stdlib/Array";
import Char "mo:stdlib/Char";
import Debug "mo:stdlib/Debug";
import Iter "mo:stdlib/Iter";
import List "mo:stdlib/List";
import Prim "mo:prim";
import Text "mo:stdlib/Text";

module {

  public module Lazy {
    public class t<A>(f : () -> A) {
      var state : { #delay : (() -> A); #result : A}
             // ^ required for correct store typing
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

    public func ofIter<A>(iter : Iter.Iter<A>) : t<A> {
      func next(iter : Iter.Iter<A>) : t<A> {
        switch (iter.next()) {
          case (?a) (?(a, Lazy.t(func () : t<A> { next iter; })));
          case null null
        };
      };
      next iter
    };

    public func ofFunc<A>(f : () -> ?A) : t<A> {
      func next(f : () -> ?A) : t<A> {
        switch (f ()) {
        case (?a) (?(a, Lazy.t(func () : t<A> { next f; })));
        case null null
        };
      };
      next f
    };

    public func ofText(t : Text) : t<Char> = ofIter(Text.toIter t);
  };

  // utils

  public func implode(cs : List.List<Char>) : Text =
  { var t = "";
    for (c in Iter.fromList cs) {
      t #= Char.toText c
    };
    t
  };

  public func explode(t : Text) : List.List<Char> {
    var l : List.List<Char> = null;
    for (c in t.chars()){
        l := List.push(c, l);
    };
    List.rev(l);
  };

  public type Input<Token> = LazyStream.t<Token>;

  public type Monad<Token, Result> = ?(Result, Input<Token>);

  public type Parser<Token, Result> = Input<Token> -> Monad<Token, Result>;

  public func ret<Token, A>(x : A) : Parser<Token, A> { func input { ?(x,input) }};

  public func bind<Token, A, B>(pa : Parser<Token, A>, f : A -> Parser<Token, B>) : Parser<Token, B> {
      func input {
        switch (pa input) {
          case (?(result,input)) (f result input);
          case null null;
        }
    }
  };

  // not in opal: used to sequence parsers
  public func pair<Token, A, B>(pa : Parser<Token, A>, pb : Parser<Token, B>) : Parser<Token, (A, B)> {
    bind(pa, (func (a : A) : Parser<Token, (A, B)> {
      bind(pb, (func (b : B) : Parser<Token, (A, B)> { ret (a,b) }))
    }))
  };

  public func choose<Token, A>(pa1 : Parser<Token, A>, pa2 : Parser<Token, A>) : Parser<Token, A> {
    func input {
      let r1 = pa1 input;
      switch r1 {
       case null (pa2 input);
       case _ r1;
     }
    }
  };

  public func mzero<Token, A>() : Parser<Token, A> = func (_ : Input<Token>) : Monad<Token, A> =  null;

  public func any<Token>(ls : Input<Token>) : Monad<Token, Token> {
    switch ls {
      case (?(token, input)) (?(token, input.force()));
      case null null;
    }
  };

  public func token<Token, A>(f : Token -> ?A) : Parser<Token, A> {
    bind((func ls = any ls) : Parser<Token, Token>, (func (res : Token) = switch (f(res)) {
      case null mzero();
      case (?a) ret a;
    }) : Token -> Parser<Token, A>)
  };

  public func satisfy<Token>(test : Token -> Bool) : Parser<Token, Token> {
    bind((func ls = any ls) // eta-expand for implicit specialization - can we do better?
          : Parser<Token, Token>,
          (func res = if (test res) (ret res) else mzero())
          : Token -> Parser<Token,Token>)

  };

  public func eof<Token, A>(a : A) : Parser<Token,A> {
    func input {
      switch input {
        case null (?(a, null));
        case _ null;
      }
    }
  };

  // not in opal: used to delay recursion
  public func delay<Token, Result>(f : () -> Parser<Token, Result>) : Parser<Token, Result> =
    func (i : Input<Token>) : Monad<Token, Result> { f () i};

  // derived

  // =>
  public func map<Token, A, B>(pa : Parser<Token, A>, f : A -> B) : Parser<Token, B> {
    bind(pa,
        (func a = ret (f a)) : A -> Parser<Token, B>);
  };

  // >>
  public func right<Token, A, B>(pa : Parser<Token, A>, pb : Parser<Token, B>) : Parser<Token, B> {
    bind(pa,
        (func _ = pb) : A -> Parser<Token, B>);
  };

  // <<
  public func left<Token, A, B>(pa : Parser<Token,A>, pb : Parser<Token, B>) : Parser<Token, A> {
    bind(
      pa,
      (func a = bind(pb,(func _ = ret a) : B -> Parser<Token, A>))
      : A -> Parser<Token, A>);
  };

  // <~>
  public func cons<Token, A>(pa : Parser<Token, A>, pas : Parser<Token, List.List<A>>) : Parser<Token, List.List<A>> {
    bind(
      pa,
      (func a = bind(pas,(func as = ret (List.push(a, as))) : List.List<A> -> Parser<Token, List.List<A>>))
      : A -> Parser<Token, List.List<A>>);
  };

  public func choice<Token, A>(ps : [Parser<Token, A>]) : Parser<Token, A> {
    func input {
      label l
      for (p in ps.vals()) {
        let r = p input;
        switch r {
          case null { continue l; };
          case _ return r;
        }
      };
      return null;
    }
  };

  public func count<Token, A>(n : Nat, pa : Parser<Token, A>) : Parser<Token, List.List<A>> {
    if (n > 0) cons(pa, count(n-1, pa))
    else ret (List.nil<A>()); // needs <A> or constraint.
  };

  public func between<Token, A, B, C>(
    pa : Parser<Token, A>,
    pb : Parser<Token, B>,
    pc : Parser<Token, C>) : Parser<Token, B> {
    right(pa, left(pb, pc));
  };

  public func option<Token, A>(default : A, pa : Parser<Token, A>) : Parser<Token, A> {
    choose(pa, ret<Token, A> default)
  };

  public func optional<Token, A>(pa : Parser<Token, A>) : Parser<Token, ()> {
    option((), right(pa, ret<Token,()>()));// needs <...> or constraint
  };

  public func skipMany<Token, A>(pa : Parser<Token, A>) : Parser<Token, ()> {
    option((), bind(pa, (func _ = skipMany pa) : A -> Parser<Token, ()>)); // needs constraint
  };

  public func skipMany1<Token, A>(pa : Parser<Token, A>) : Parser<Token, ()> {
    right(pa, skipMany pa)
  };

  public func many<Token, A>(pa : Parser<Token, A>) : Parser<Token, List.List<A>> {
    option(List.nil<A>(),
      bind(pa,
      (func a {
        bind(many pa,
              (func as { ret (List.push(a,as)) }) :
              List.List<A> -> Parser<Token, List.List<A>>) }) // needs constraint
        : A -> Parser<Token, List.List<A>> )) // needs constraint
  };

  public func many1<Token, A>(pa : Parser<Token, A>) : Parser<Token, List.List<A>> {
    cons(pa, many pa);
  };

  public func sepBy1<Token, A, B>(pa : Parser<Token, A>, sep : Parser<Token, B>)
    : Parser<Token,List.List<A>> {
    cons(pa, many(right(sep, pa)))
  };

  public func sepBy<Token, A, B>(pa : Parser<Token, A>, sep : Parser<Token, B>)
    : Parser<Token, List.List<A>> {
    choose(sepBy1(pa, sep), ret<Token, List.List<A>>(List.nil())) // NB: can't infer but need to provide <...>
  };

  public func endBy1<Token, A, B>(pa : Parser<Token, A>, sep : Parser<Token, B>)
    : Parser<Token, List.List<A>> {
    left(sepBy1(pa, sep), sep)
  };

  public func endBy<Token, A, B>(pa : Parser<Token, A>, sep : Parser<Token, B>)
    : Parser<Token, List.List<A>> {
    choose(endBy1(pa, sep), ret<Token, List.List<A>>(List.nil())) // NB: can't infer but need to <...>
  };

  public func chainl1<Token, A, B>(
    pa : Parser<Token, A>,
    op : Parser<Token, (A, A) -> A>)
    : Parser<Token, A> {
    func iter(a : A) : Parser<Token, A> {
      choose(
        bind(
         pair(op, pa),
         func ((f : (A, A) -> A, b : A)) : Parser<Token, A> { iter (f(a, b)) }),
        ret<Token, A> a)
    };
    bind(pa, iter)
  };

  public func chainl<Token, A, B>(
    pa : Parser<Token, A>,
    op: Parser<Token, (A, A) -> A>,
    default : A) : Parser<Token, A> {
    choose(
      chainl1(pa, op),
      ret<Token, A> default)
  };

  public func chainr1<Token, A, B>(
    pa : Parser<Token, A>,
    op: Parser<Token, (A, A) -> A>)
    : Parser<Token, A> {
    bind(pa,
      func (a : A) :  Parser<Token, A>
      { bind(
          op,
          func (f : (A, A) -> A) : Parser<Token, A> {
            choose(
              map(chainr1(pa, op), func (a2 : A) : A { f (a, a2)}),
              ret<Token, A> a)
          })
      })
  };

  public func chainr<Token, A, B>(
    pa : Parser<Token, A>,
    op: Parser<Token, (A, A) -> A>,
    default : A) : Parser<Token, A> {
    choose(
      chainr1(pa, op),
      ret<Token, A> default)
  };

  // singletons (need to pass in eq/leq)
  public func exactly<Token>(eq : (Token, Token) -> Bool, t : Token) : Parser<Token, Token> {
    satisfy (func (t1 : Token) : Bool = eq(t, t1))
  };

  public func oneOf<Token>(eq : (Token, Token)-> Bool, tokens : [Token]) : Parser<Token, Token>  {
    satisfy (func (t : Token) : Bool { 
      for (t1 in tokens.vals()) {
        if (eq(t, t1)) { return true }
      };
      return false
    })
  };

  public func noneOf<Token>(eq : (Token, Token) -> Bool, tokens : [Token]) : Parser<Token, Token>  {
    satisfy (func (t : Token) : Bool {
      for (t1 in tokens.vals()) {
        if (eq(t, t1)) { return false }
      };
      return true;
    })
  };

  public func range<Token>(leq : (Token, Token) -> Bool, l : Token, r : Token) : Parser<Token, Token>  {
    satisfy (func (t : Token) : Bool { leq(l, t) and leq(t, r) })
  };

  // Char parsers
  // place in a class otherwise rejected as non-static
  // alternative is to eta-expand each one
  // annoyance: all decs must be typed in order to refer to previous outer combinators
  //            is that a bug?

  public class CharParsers() {

    func eq(c1 : Char, c2 : Char) : Bool { c1 == c2 };

    func leq(c1 : Char, c2 : Char) : Bool { c1 <= c2 };

    public let space : Parser<Char, Char> =
      oneOf<Char>(func (c1, c2) { c1 == c2 },
       [' ', '\t', '\r', '\n']);

    public let spaces : Parser<Char, ()> = skipMany space;

    public let newline : Parser<Char, Char> = exactly(eq, '\n');

    public let tab : Parser<Char, Char> = exactly(eq, '\n');

    public let upper : Parser<Char, Char> = range(leq, 'A', 'Z');

    public let lower : Parser<Char, Char> = range(leq, 'a', 'z');

    public let digit : Parser<Char, Char> = range(leq, '0', '9');

    public let letter : Parser<Char, Char> = choose(lower, upper);

    public let alpha_num : Parser<Char, Char> = choose(letter, digit);

    public let hex_digit : Parser<Char, Char> =
      choose(range(leq, 'a', 'f'),
        choose (range(leq, 'A', 'F'),
          digit));

    public let oct_digit : Parser<Char, Char> = range(leq, '0', '7');

    public func lexeme<A>(pa : Parser<Char, A>) : Parser<Char, A> {
      right(spaces, pa)
    };

    public func token(t : Text) : Parser<Char, Text> {
      func iter(i : Iter.Iter<Char>) : Parser<Char, Text> {
          switch (i.next()) {
            case null (ret t);
            case (?c) (right(exactly(eq, c), iter i));
          }
      };
      lexeme(iter(t.chars()));
    }

  }
}
