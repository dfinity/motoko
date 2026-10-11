//MOC-FLAG --plugin ../../../_build/compute-plugins/candidx -A=M0210
import Prim "mo:⛔";

var effects = "";
let result = (with cycles = do { effects #= "note;"; 0 })
  (do { effects #= "callee;"; Prim.to_candid_canonical })(
    do { effects #= "argument;"; 7 : Nat }
  );
assert effects == "note;callee;argument;";
assert result == Prim.to_candid_canonical(7 : Nat);
