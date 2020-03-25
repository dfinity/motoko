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

type LC =
  { #lcvar: Text
  ; #lcapp: { fun: LC; arg: LC }
  ; #lclam: { binder: Text; body: LC }
  };

func lcvar(v: Text): LC {
  #lcvar(v)
};

func lclam(binder_: Text, body_: LC): LC {
  #lclam { binder = binder_; body = body_ }
};

func lcapp(fun_: LC, arg_: LC): LC {
  #lcapp{ fun = fun_; arg = arg_ }
};

func parensIf(b: Bool, t: Text): Text {
  if(b) {
    "(" # t # ")"
  } else {
    t
  }
};

func printLC(lc: LC): Text {
  printInner(false, lc)
};

func printInner(parens: Bool, lc: LC): Text {
  switch (lc) {
    case (#lcvar(v)) v;
    case (#lcapp{fun; arg}) parensIf(parens, printInner(true, fun) # " " # printLC(arg));
    case (#lclam{binder; body}) parensIf(parens, "\\" # binder # ". " # printLC(body));
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


func showToken(tkn: Token): Text =
  switch(tkn) {
  case (#lam) "LAM";
  case (#dot) "DOT";
  case (#lparen) "LPAREN";
  case (#rparen) "RPAREN";
  case (#ident(i)) "IDENT(" # i # ")";
  };


func printToken(tkn: Token): () = Debug.print(showToken(tkn));

module LCParser = {

  type LCParser = P.Parser<Token, LC>;

  func parseVar() : LCParser {
    P.map(ident, lcvar)
  };

  func parseParens() : LCParser {
    P.between(lparen, P.delay parseLC, rparen);
  };

  func parseLambda(): LCParser {
    P.map(P.pair(P.between(lam, ident, dot), P.delay parseLC),
      func ((v, lc) : (Text,LC)) : LC { lclam(v,lc) });
  };

  public func parseAtom(): LCParser  {
    P.choice([
      parseParens(),
      parseVar(),
      parseLambda()
    ])
  };

  public func parseLC(): LCParser {
    P.map(P.pair(parseAtom(), P.many(parseAtom())),
      func ((lc, lcs) : (LC, List.List<LC>)) : LC {
        List.foldLeft(lcs, lc, func (arg:LC, acc:LC) : LC = lcapp(acc, arg))
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
  switch (LCParser.parseLC()(input)) {
    case (?(lc, _)) Debug.print(printLC(lc));
    case null Debug.print("Failed to parse");
  }
};

main();
