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
