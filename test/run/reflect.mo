import {debugPrint} = "mo:⛔";

type Desc<A> = { #int : A -> Int; #char : A -> Char; #pair : A -> (Desc<A>, Desc<A>) };

func foo</*switch*/ A>(a : A, da : Desc<A>) : Text {
    switch da {
        case (#int f) { "It's an Int: " # debug_show f a };
        case (#char f) { "It's a Char: " # debug_show f a };
        case (#pair f) { "It's a pair: (" # foo(a, (f a).0) # ", " # foo(a, (f a).1) # ")" }
    }
};

debugPrint(foo(-42, #int (func (a : Int) : Int = a)));
debugPrint(foo('X', #char (func (c : Char) : Char = c)));
debugPrint(foo((-5, 'C'), #pair (func (p : (Int, Char)) : (Desc<(Int, Char)>, Desc<(Int, Char)>) = (#int (func (p : (Int, Char)) : Int = p.0), #char (func (p : (Int, Char)) : Char = p.1)))));

// precompose `f`
func pre<A, B>(f : A -> B, d : Desc<B>) : Desc<A> =
         switch d {
             case (#int g) { #int (func (a : A) : Int = g (f a)) };
             case (#char g) { #char (func (a : A) : Char = g (f a)) };
             case (#pair g) { #pair (func (a : A) : (Desc<A>, Desc<A>) {
                                         let (d0, d1) = g (f a);
                                         (pre(f, d0), pre(f, d1))
                                     })
                  }
         };

func pair_wrap<A>(d : Desc<A>) : Desc<(A, Char)> {
    #pair (func ((_ : A, c : Char)) = (pre(func (p : (A, Char)) : A = p.0, d), #char (func (p : (A, Char)) : Char = p.1)))
};

func bar</*switch*/ A>(a : A, da : Desc<A>) : Text {
    foo((a, 'M'), pair_wrap da)
};

debugPrint(bar(-25, #int (func (a : Int) : Int = a)));
