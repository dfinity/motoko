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

let lam = P.token(func (t : Token) : ?() { switch t {
  case (#lam) ?();
  case _ null;
}});

let dot = P.token(func (t : Token) : ?() { switch t {
  case (#dot) ?();
  case _ null;
}});

let lparen = P.token(func (t : Token) : ?() { switch t {
  case (#lparen) ?();
  case _ null;
}});

let rparen = P.token(func (t : Token) : ?() { switch t {
  case (#rparen) ?();
  case _ null;
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
    P.choice(List.fromArray([
      parseParens(),
      parseVar(),
      parseLambda(),
    ]))
  };

  public func parseLC(): LCParser {
    P.map(P.pair(parseAtom(), P.many(parseAtom())),
      func ((lc, lcs) : (LC, List.List<LC>)) : LC {
        List.foldLeft(lcs, lc, func (arg:LC, acc:LC) : LC = lcapp(acc, arg))
      });
  }

};

let lex: Text -> ?[Token] = func(input) {
  var lookahead: ?Char = null;
  let inputIter = Text.toIter(input);
  var tokens: [Token] = [];

  func peek(): ?Char {
    lookahead := next();
    lookahead
  };

  func next(): ?Char {
    let res = switch(lookahead) {
      case null inputIter.next();
      case (?(v)) {
      lookahead := null;
      ?v
      }
    };
    // Debug.print("Next is: " # Option.option(res, Char.toText, "None"));
    res
  };

  func bump() {
    let _ = next();
    ()
  };

  func isWhitespace(c : Char) : Bool {
    c == ' ' or c == '\n' or c == '\t' or c == '\r'
  };

  func isIdentStart(c : Char) : Bool {
    c >= 'A' and c <= 'z'
  };

  func isIdent(c : Char) : Bool {
    isIdentStart(c) or Char.isDigit(c)
  };

  func recognize(token : Token) {
    tokens := Array.append(tokens, [token])
  };

  func dropWS() {
    while(Option.option(peek(), isWhitespace, false)) bump()
  };

  dropWS();
  while(true) {
    switch(next()) {
    case null return ?tokens;
    case (?('\\')) recognize(#lam);
    case (?('.')) recognize(#dot);
    case (?('(')) recognize(#lparen);
    case (?(')')) recognize(#rparen);
    case (?c) {
      if (isIdentStart(c)) {
      var ident = Char.toText(c);
      while(Option.option(peek(), isIdent, false)) {
        ident #= Char.toText(Option.unwrap(next()))
      };
      recognize(#ident ident)
      } else {
      Debug.print("Failed to handle: " # Char.toText(c));
      return null;
      }
    };
    };
    dropWS();
  };
  ?tokens
};

let cp = P.CharParsers();

let lexIdent: P.Parser<Char, Token> = cp.lexeme(
  P.bind(
  cp.letter,
  func(c: Char): P.Parser<Char, Token> {
    P.map(
    P.many(cp.alpha_num),
    func (cs: List.List<Char>): Token {
      let ident: Text =
      List.foldLeft(
        cs,
        Char.toText c,
        func (c: Char, acc: Text): Text = acc # Char.toText(c)
      );
      #ident ident
    }
    )
  }
  )
);

func runCharParser<A>(parser: P.Parser<Char, A>, input: Text): A {
  let (res, _) = Option.unwrap(parser(P.LazyStream.ofText(input)));
  res
};

let lexCombinator = func(input : Text) : ?[Token] {

  func const<A>(a: A): Any -> A = func(_) = a;

  let token = func(t: Text, tkn: Token): P.Parser<Char, Token> =
    P.map<Char, Text, Token>(cp.token(t), const(tkn));

  let parser = P.many(
  P.choice(
    List.fromArray([
    lexIdent,
    token(".", #dot),
    token("\\", #lam),
    token("(", #lparen),
    token(")", #rparen),
    ]))
  );

  switch (parser(P.LazyStream.ofText(input))) {
  case (?(tkns, _)) ?(List.toArray(tkns));
  case null null;
  }
};


func main() {
  // Parses the Omega combinator
  // let identParse = runCharParser(lexIdent, "lol");
  // Debug.print("Ident parse:");
  // Debug.print("=====================");
  // printToken identParse;

  let tokensImp = Option.unwrap(lex("(\\x. x x) (\\x. x x)"));
  Debug.print("Imperative lex:");
  Debug.print("=====================");
  let _ = Array.map<Token, ()>(func t = printToken t, tokensImp);

  let tokens: [Token] = Option.unwrap(lexCombinator("(\\x. x x) (\\x. x x)"));
  Debug.print("Combinator based lex:");
  Debug.print("=====================");
  let _ = Array.map<Token, ()>(func t = printToken t, tokens);

  let input = P.LazyStream.ofIter(Iter.fromArray(tokens));
  switch (LCParser.parseLC()(input)) {
  case (?(lc, _)) Debug.print(printLC(lc));
  case null Debug.print("Failed to parse");
  }
};

main();
