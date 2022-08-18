// test that object extension happens at the type plane too

object a { public type T = Int; public let x : T = 42 };
object b { public type T = Int; public let y : () -> T = func _ = 42 };
object c { public type X = Int; public let y : () -> X = func _ = 42 };

let ac = { a and c };

// test that type-level `and` also works with type fields
//
//     But: we cannot declare record types with type fields in them, yet...
// let ac1 : {type T = Int; type X = Int} and {x : Int; y : () -> Int} = ac;

// T = Int and T = Int are reconcilable
//let ab = { a and b }; // FIXME: gives 'type error [M0177], ambiguous field in base'

module M { public type U = Nat; public let u : U = 25  };
let am = { a with M };
let bm = { b with M };

let aM = { a and M };
let bM = { b and M };

// poor man's type definition to declare object type with type fields
class C() {
 public type T = Int;
 public let t : Bool = true;
};

let r1 = module { public type t = Nat; };
ignore ({ r1 with t = true } : C); // check field t doesn't hide type field t;
