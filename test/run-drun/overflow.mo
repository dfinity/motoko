import Prim "mo:prim";
// We have these tests in run-drun because we want to check that certain
// traps are happening, and a good way to test this is if a message gets
// aborted.

actor a {
  public func go() : async (){
    try {await async {
        ignore ((0-1):Int);
        Prim.debugPrint("ok 1");
    }} catch e {
        Prim.debugPrint("not ok 1");
    };
    try {await async {
        ignore ((1-+1):Nat);
        Prim.debugPrint("ok 2");
    }} catch e {
        Prim.debugPrint("not ok 2");
    };
    try {await async {
        ignore ((0-+1):Nat);
        Prim.debugPrint("not ok 3");
    }} catch e {
        Prim.debugPrint("ok 3");
    };
    try {await async {
        ignore ((18446744073709551615 + 0):Nat);
        Prim.debugPrint("ok 4");
    }} catch e {
        Prim.debugPrint("not ok 4");
    };
    try {await async {
        ignore ((9223372036854775806 + 9223372036854775806 + 1):Nat);
        Prim.debugPrint("ok 5");
    }} catch e {
        Prim.debugPrint("not ok 5");
    };
    try {await async {
        ignore ((9223372036854775806 + 9223372036854775806 + 2):Nat);
        Prim.debugPrint("ok 6");
    }} catch e {
        Prim.debugPrint("not ok 6");
    };
  }
};
a.go(); //OR-CALL ingress go "DIDL\x00\x00"

// not useful, we don't do rollback properly in the interpreter
//SKIP run
//SKIP run-ir
//SKIP run-low
