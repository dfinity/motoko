// compile and run with
// ../src/moc -wasi-system-api --package stdlib ../stdlib/src lambda-calculus.mo && wasmtime lambda-calculus.wasm
import Debug "mo:stdlib/debug";
import Iter "mo:stdlib/iter";
import List "mo:stdlib/list";
import Option "mo:stdlib/option";
import P "parsec";

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

let lam = P.token(func (t:Token) :? () { switch t {
    case (#lam) ?();
    case _ null;
}});

let dot = P.token(func (t:Token) :? () { switch t {
    case (#dot) ?();
    case _ null;
}});

let lparen = P.token(func (t:Token) : ? () { switch t {
    case (#lparen) ?();
    case _ null;
}});

let rparen = P.token(func (t:Token) : ?() { switch t {
    case (#rparen) ?();
    case _ null;
}});

class LCParser() {
    type LCParser = P.Parser<Token, LC>;
    let parseVar : LCParser = P.then(ident, func (v:Text) : LC = #lcvar(v));
    
    func parseParens_(): LCParser {
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
    };

    func delay<Token, Result>(f:() -> P.Parser<Token,Result>) : P.Parser<Token,Result> = func (i:P.Input<Token>) : P.Monad<Token,Result> { f () i};

    func parseParens() : LCParser =
        P.bind(P.between(lparen, rparen, delay parseLC),
            func (lc: LC): LCParser {  P.ret lc });

    func parseLambda(): LCParser = 
        P.bind(P.pair(P.between(lam,dot,ident),delay parseLC),
               func (p:(Text,LC)) : LCParser { P.ret (lclam(p.0,p.1)) });
               

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


func main() {
    let input =
        P.LazyStream.ofIter(
            Iter.fromArray(
                [ #lparen, #lam, #ident "x", #dot, #ident "x", #ident "x", #rparen
                , #lparen, #lam, #ident "x", #dot, #ident "x", #ident "x", #rparen
                ]
            )
        );
    switch (LCParser().parseLC()(input)) {
        case (?(lc, _)) Debug.print(printLC(lc));
        case null Debug.print("Failed to parse");
    }
};

main();
