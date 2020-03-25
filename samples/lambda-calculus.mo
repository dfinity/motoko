// compile and run with
// ../src/moc -wasi-system-api --package stdlib ../stdlib/src lambda-calculus.mo && wasmtime lambda-calculus.wasm
import Array "mo:stdlib/array";
import Char "mo:stdlib/char";
import Debug "mo:stdlib/debug";
import Iter "mo:stdlib/iter";
import List "mo:stdlib/list";
import Option "mo:stdlib/option";
import P "parsec";
import Prim "mo:prim";
import Text "mo:stdlib/text";

type Id = Text;

type Term =
  { #id : Id
  ; #app : (Term, Term)
  ; #lam : (Id, Term)
  };

func id(v: Id): Term {
  #id(v)
};

func lambda(id : Id, body : Term) : Term {
  #lam (id, body)
};

func app(fun : Term, arg : Term) : Term {
  #app(fun,  arg)
};

func parensIf(b: Bool, t: Text) : Text {
  if (b) {
    "(" # t # ")"
  } else {
    t
  }
};

func printTerm(tm : Term) : Text {
  printInner(false, tm)
};

func printInner(parens : Bool, tm : Term) : Text {
  switch (tm) {
    case (#id(v)) v;
    case (#app(fun, arg)) parensIf(parens, printInner(true, fun) # " " # printTerm(arg));
    case (#lam(id, body)) parensIf(parens, "\\" # id # ". " # printTerm(body));
  }
};

type Token = { #lam; #dot; #lparen; #rparen; #ident: Text; };

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


func showToken(tkn : Token) : Text =
  switch(tkn) {
  case (#lam) "LAM";
  case (#dot) "DOT";
  case (#lparen) "LPAREN";
  case (#rparen) "RPAREN";
  case (#ident(i)) "IDENT(" # i # ")";
  };


func printToken(tkn: Token): () = Debug.print(showToken(tkn));

module TermParser = {

  type TermParser = P.Parser<Token, Term>;

  func parseId() : TermParser {
    P.map(ident, id)
  };

  func parseParens() : TermParser {
    P.between(lparen, P.delay parseTerm, rparen);
  };

  func parseLambda(): TermParser {
    P.map(P.pair(P.between(lam, ident, dot), P.delay parseTerm),
      func ((v, tm) : (Text,Term)) : Term { lambda(v,tm) });
  };

  public func parseAtom(): TermParser  {
    P.choice([
      parseParens(),
      parseId(),
      parseLambda()
    ])
  };

  public func parseTerm(): TermParser {
    P.map(P.pair(parseAtom(), P.many(parseAtom())),
      func ((tm, tms) : (Term, List.List<Term>)) : Term {
        List.foldLeft(tms, tm, func (arg:Term, acc:Term) : Term = app(acc, arg))
      });
  }

};

let tokens = func(input : Text) : Iter.Iter<Token> {
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

func main() {
  // Parses the Omega combinator
  let omega = "(\\x. x x) (\\x. x x)";

  // tokenize
  for (token in tokens omega) {
    printToken token;
  };

  // parse
  let input = P.LazyStream.ofIter(tokens omega);
  switch (TermParser.parseTerm()(input)) {
    case (?(tm, _)) Debug.print(printTerm(tm));
    case null Debug.print("Failed to parse");
  }
};

main();
