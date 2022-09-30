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

// fmap
func prep<A, B>(f : A -> B, d : Desc<B>) : Desc<A> =
         switch d {
         case (#int g) { #int (func (a : A) : Int = g (f a)) };
         case (#char g) { #char (func (a : A) : Char = g (f a)) }
         };

func pair_wrap<A>(d : Desc<A>) : Desc<(A, Char)> {
    #pair (func ((_ : A, c : Char)) = (prep(func (p : (A, Char)) : A = p.0, d), #char (func ((_ : A, c : Char)) : Char = c)))
};

func bar</*switch*/ A>(a : A, da : Desc<A>) : Text {
    foo((a, 'M'), pair_wrap da)
};
