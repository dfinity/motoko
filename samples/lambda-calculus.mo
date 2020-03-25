// compile and run with
// ../src/moc -wasi-system-api --package stdlib ../stdlib/src lambda-calculus.mo && wasmtime lambda-calculus.wasm
import Array "mo:stdlib/Array";
import Char "mo:stdlib/Char";
import Debug "mo:stdlib/Debug";
import Iter "mo:stdlib/Iter";
import List "mo:stdlib/List";
import Option "mo:stdlib/Option";
import P "parsec";
import Prim "mo:prim";
import Text "mo:stdlib/Text";

module Syntax {

  public type Id = Text;

  public type Term =
    { #id : Id
    ; #app : (Term, Term)
    ; #lam : (Id, Term)
    };

  public func id(v: Id): Term {
    #id(v)
  };

  public func lambda(id : Id, body : Term) : Term {
    #lam (id, body)
  };

  public func app(fun : Term, arg : Term) : Term {
    #app(fun,  arg)
  };

  func parensIf(b: Bool, t: Text) : Text {
    if (b) {
      "(" # t # ")"
    } else {
      t
    }
  };

  func printInner(parens : Bool, tm : Term) : Text {
    switch (tm) {
      case (#id(v)) v;
      case (#app(fun, arg)) parensIf(parens, printInner(true, fun) # " " # printTerm(arg));
      case (#lam(id, body)) parensIf(parens, "\\" # id # ". " # printTerm(body));
    }
  };

  public func printTerm(tm : Term) : Text {
    printInner(false, tm)
  };
};

module Lexer = {
  public type Token = { #lam; #dot; #lparen; #rparen; #ident: Text; };

  func showToken(tkn : Token) : Text =
    switch(tkn) {
    case (#lam) "LAM";
    case (#dot) "DOT";
    case (#lparen) "LPAREN";
    case (#rparen) "RPAREN";
    case (#ident(i)) "IDENT(" # i # ")";
    };


  public func printToken(tkn: Token) {
    Debug.print(showToken(tkn))
  };

  public func tokens(input : Text) : Iter.Iter<Token> {
    let cp = P.CharParsers();

    func text(t : Text, token: Token): P.Parser<Char, Token> {
      P.right(cp.token(t), P.ret<Char,Token>(token))
    };

    func ident(f : Text -> Token) : P.Parser<Char,Token> {
      P.map(
        P.right(cp.spaces, P.cons(cp.letter, P.many(cp.alpha_num))),
        func (cs : List.List<Char>) : Token { f (P.implode(cs)) });
    };

    let token = P.choice([
          ident(func t { #ident t }),
          text(".", #dot),
          text("\\", #lam),
          text("(", #lparen),
          text(")", #rparen)]);

    var state = P.LazyStream.ofText(input) ;

    object {
      public func next() : ?Token {
        switch (token(state)) {
          case (?(tok, rest)) {
            state := rest;
            ? tok
          };
          case null null;
        }
      }
    }
  };
};


class Parser() {

  type Token = Lexer.Token;
  type Term  = Syntax.Term;

  type Parser = P.Parser<Token, Syntax.Term>;

  let ident = P.token(func (t : Token) : ?Text { switch t {
     case (#ident(i)) ?i;
     case _ null;
   }});

  let lam = P.satisfy(func (t : Token) : Bool { switch t {
     case (#lam) true;
     case _ false;
   }});

  let dot = P.satisfy(func (t : Token) : Bool { switch t {
     case (#dot) true;
     case _ false;
  }});

  let lparen = P.satisfy(func (t : Token) : Bool { switch t {
    case (#lparen) true;
    case _ false;
  }});

  let rparen = P.satisfy(func (t : Token) : Bool { switch t {
    case (#rparen) true;
    case _ false;
  }});

  func id() : Parser = P.map(ident, Syntax.id);
  
  func parens() : Parser = P.between(lparen, P.delay term, rparen);
  
  func lambda() : Parser = 
    P.map(P.pair(P.between(lam, ident, dot), P.delay term),
      func ((v, tm) : (Text,Term)) : Term { Syntax.lambda(v, tm) });

  func atom() : Parser =
    P.choice([
      parens(),
      id(),
      lambda()
    ]);
  
  public func term() : Parser =
    P.map(P.pair(atom(), P.many(atom())),
      func ((tm, tms) : (Term, List.List<Term>)) : Term {
        List.foldLeft(tms, tm, func (arg:Term, acc:Term) : Term = Syntax.app(acc, arg))
      });

};

// Lex and parse the omega combinator
func test() {

  let omega = "(\\x. x x) (\\x. x x)";

  // tokenize
  for (token in Lexer.tokens omega) {
    Lexer.printToken token;
  };

  // parse
  let parse = Parser().term();
  let input = P.LazyStream.ofIter(Lexer.tokens omega);
  switch (parse(input)) {
    case (?(tm, _)) Debug.print(Syntax.printTerm(tm));
    case null Debug.print("Failed to parse");
  }
};

test();
