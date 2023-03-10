import Prim "mo:⛔";

func returns_tuple() : (Nat, Nat) {
  return (1,2);
};

func multi_value_if(n : Nat) : (Nat, Nat) {
  if (n == 0) {
    returns_tuple()
  } else {
    if (n == 1) {
      returns_tuple()
    } else {
      (3,4);
    }
 }
};


func multi_value_switch(n : Nat) : (Nat, Nat) {
  switch (n){
    case (0) { returns_tuple() };
    case (1) { returns_tuple() };
    case (_) { (5,6) };
  };
};



let a0 = Prim.rts_total_allocation();
let (x1,y1) = returns_tuple();
let a1 = Prim.rts_total_allocation();
let (x2,y2) = multi_value_if(0);
let a2 = Prim.rts_total_allocation();
let (x3,y3) = multi_value_if(1);
let a3 = Prim.rts_total_allocation();
let (x4,y4) = multi_value_if(3);
let a4 = Prim.rts_total_allocation();
let (x5,y5) = multi_value_switch(0);
let a5 = Prim.rts_total_allocation();
let (x6,y6) = multi_value_switch(1);
let a6 = Prim.rts_total_allocation();
let (x7,y7) = multi_value_switch(3);
let a7 = Prim.rts_total_allocation();

assert((x1,y1) == (1,2));
assert((x2,y2) == (1,2));
assert((x3,y3) == (1,2));
assert((x4,y4) == (3,4));
assert((x5,y5) == (1,2));
assert((x6,y6) == (1,2));
assert((x7,y7) == (5,6));
assert(a1 == a0);
assert(a2 == a0);
assert(a3 == a0);
assert(a4 == a0);
assert(a5 == a0);
assert(a6 == a0);
assert(a7 == a0);
