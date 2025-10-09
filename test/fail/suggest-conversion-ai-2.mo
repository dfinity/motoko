//MOC-FLAG --package conv conv --ai-errors --ai-implicit-libs
// should report conversions from all available libs, even when not imported
// test that `--ai-implicit-libs` does not break the suggestions
import Nat8 = "mo:conv/Nat8"
func f(n : Nat) {};
let n8 : Nat8 = 0;
let n16 : Nat16 = 0;
f(n8);
f(n16);


