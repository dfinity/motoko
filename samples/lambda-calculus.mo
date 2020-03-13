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

module LCParser = {

    type LCParser = P.Parser<Token, LC>;
    func parseVar() : LCParser { 
        P.then(ident, func (v:Text) : LC = #lcvar(v))
    };
    
    func parseParens() : LCParser =
        P.then(P.between(lparen, P.delay parseLC, rparen),
            func (lc : LC) : LC {  lc });

    func parseLambda(): LCParser = 
        P.then(P.pair(P.between(lam, ident, dot), P.delay parseLC),
               func ((v, lc) : (Text,LC)) : LC { lclam(v,lc) });
               

    public func parseAtom(): LCParser  {
        P.choice(List.fromArray([
            parseParens(),
            parseVar(),
            parseLambda(),
    ]))};

    public func parseLC(): LCParser {
        P.then(P.pair(parseAtom(), P.many(parseAtom())),
            func ((lc, lcs) : (LC, List.List<LC>)) : LC {
                List.foldLeft(lcs, lc, func (arg:LC, acc:LC) : LC = lcapp(acc, arg))
            });
    }

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
    switch (LCParser.parseLC()(input)) {
        case (?(lc, _)) Debug.print(printLC(lc));
        case null Debug.print("Failed to parse");
    }
};

main();
