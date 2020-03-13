// compile and run with
// ../src/moc -wasi-system-api --package stdlib ../stdlib/src lambda-calculus.mo && wasmtime lambda-calculus.wasm
import Array "mo:stdlib/array";
import Char "mo:stdlib/char";
import Debug "mo:stdlib/debug";
import Iter "mo:stdlib/iter";
import List "mo:stdlib/list";
import Option "mo:stdlib/option";
import P "parsec";
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

class LCParser() {
    type LCParser = P.Parser<Token, LC>;
    let parseVar : LCParser = P.then(ident, func (v:Text) : LC = #lcvar(v));
    func parseParens(): LCParser =
        // Unfortunately between loops here?
        // P.between<Token, (), (), LC>(lparen, rparen, parseLC());
        P.bind(
            lparen,
            func (_:()) : LCParser = P.bind(
                parseLC(),
                func (lc : LC) : LCParser = P.bind(
                    rparen,
                    func (_:()) : LCParser = P.ret(lc)
                )
            )
        );

    func parseLambda(): LCParser = P.bind(
        lam,
        func (_: ())  : LCParser { P.bind(
            ident,
            func (binder:Text) : LCParser { P.bind(
                dot,
                func (_:()) : LCParser { P.bind(
                    parseLC(),
                    func (body:LC) : LCParser = P.ret(lclam(binder, body))
                )}
            )}

        )
        }
    );

    public func parseAtom(): LCParser = P.choice(List.fromArray([
        parseParens(),
        parseVar,
        parseLambda(),
    ]));

    public func parseLC(): LCParser =
      P.bind(
            P.many1(parseAtom()),
            func (atoms : List.List<LC>) : LCParser {
                let (hd, args) = List.pop(atoms);
                let fn = Option.unwrap(hd);
                P.ret(List.foldLeft(args, fn, func (arg:LC, acc:LC) : LC = lcapp(acc, arg)))
            });
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

func main() {
    // Parses the Omega combinator
    let tokens = Option.unwrap(lex("(\\x. x x) (\\x. x x)"));
    let input = P.LazyStream.ofIter(Iter.fromArray(tokens));
    switch (LCParser().parseLC()(input)) {
        case (?(lc, _)) Debug.print(printLC(lc));
        case null Debug.print("Failed to parse");
    }
};

main();
