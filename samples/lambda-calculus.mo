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

let ident: P.Parser<Token, Text> = P.token<Token, Text>(func t = switch t {
    case (#ident(i)) ?i;
    case _ null;
});

let lam: P.Parser<Token, ()> = P.token<Token, ()>(func t = switch t {
    case (#lam) ?();
    case _ null;
});

let dot: P.Parser<Token, ()> = P.token<Token, ()>(func t = switch t {
    case (#dot) ?();
    case _ null;
});

let lparen: P.Parser<Token, ()> = P.token<Token, ()>(func t = switch t {
    case (#lparen) ?();
    case _ null;
});

let rparen: P.Parser<Token, ()> = P.token<Token, ()>(func t = switch t {
    case (#rparen) ?();
    case _ null;
});

class LCParser() {
    let parseVar: P.Parser<Token, LC> = P.then<Token, Text, LC>(ident, func v = #lcvar(v));
    func parseParens(): P.Parser<Token, LC> =
        // Unfortunately between loops here?
        // P.between<Token, (), (), LC>(lparen, rparen, parseLC());
        P.bind<Token, (), LC>(
            lparen,
            func () = P.bind<Token, LC, LC>(
                parseLC(),
                func lc = P.bind<Token, (), LC>(
                    rparen,
                    func () = P.ret(lc)
                )
            )
        );

    func parseLambda(): P.Parser<Token, LC> = P.bind<Token, (), LC>(
        lam,
        func () = P.bind<Token, Text, LC>(
            ident,
            func binder = P.bind<Token, (), LC>(
                dot,
                func () = P.bind<Token, LC, LC>(
                    parseLC(),
                    func body = P.ret(lclam(binder, body))
                )
            )
        )
    );

    public func parseAtom(): P.Parser<Token, LC> = P.choice (List.fromArray([
        parseParens(),
        parseVar,
        parseLambda(),
    ]));

    public func parseLC(): P.Parser<Token, LC> =
      P.bind<Token, List.List<LC>, LC>(
            P.many1(parseAtom()),
            func atoms = {
                let (hd, args) = List.pop(atoms);
                let fn = Option.unwrap(hd);
                P.ret(List.foldLeft<LC, LC>(args, fn, func (arg, acc) = lcapp(acc, arg)))
            });
};


func main() {
    let input: P.Input<Token> =
        P.LazyStream.ofIter(
            Iter.fromArray<Token>(
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
