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
