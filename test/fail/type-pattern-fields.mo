import { type T; type Result } "type-pattern-fields/lib";

let _t : T = 10;
let _r1 : Result<Nat, Text> = #ok(3);
let _r2 : Result<Nat, Text> = #err("Woot");

type L1 = module { type N = Nat };
type R1 = module { type N = Text };
type V1 = { #left : L1; #right : R1 };

func _f1(x : V1) {
  // Fine
  let (#left{ type N }) = x else {
    return ()
  };

  let _n : N = 10;
};

func _f1_1(x : V1) {
  // Error with mismatched types
  let (#left{ type N } or #right { type N }) = x else {
    return ()
  };
  let _n : N = 10;
};

type L2 = module { type N = Nat };
type R2 = module { type N<A> = Nat };
type V2 = { #left : L2; #right : R2 };

func _f2(x : V2) {
  // Error with mismatched types
  let (#left{ type N } or #right { type N }) = x else {
    return ()
  };
  let _n : N = 10;
};

func _f2_1(x : V2) {
  // Error for different set of type bindings in patterns
  let (#left{ type N } or #right { }) = x else {
    return ()
  };
  let _n : N = 10;
};

type T3 = module { type N = Nat; x : module { type S = Text } };
func _f3(x : T3) {
  let ({ type N; x = { type S } }) = x;
  let t : N = 10;
  let s : S = "hi";
};

type T4 = (module { type N = Nat; }, Nat);
func _f4(x : T4) {
  let ({ type N }, _) = x;
  let t : N = 10;
};

type T5 = [module { type N = Nat; y : Nat }];
func _f5(x : T5) {
  for ({ type N; y } in x.values()) {
    let z : N = 10;
  };
};

type T6 = module { type N = Nat };
func _f6({ type N } : T6) {
  let x : N = 10;
};

type T7 = module { type N<A> = A };
func _f7({ type N } : T7) {
  let x : N<Text> = 10;
};

// func _f8() {
//   let { type K } = module { type K = M };
//   let { type M } = module { type M = K };
// };
