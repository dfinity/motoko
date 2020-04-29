import Prim "mo:prim";
{
var output : Text = "";
var i = 1000000;
while (i > 0) {
  output := "WHOOHOO\n" # output;
  i -= 1;
};
Prim.debugPrint(output);
};

{
var output : Text = "";
var i = 1000000;
while (i > 0) {
  output := "WHOOHOO\n" # output;
  i -= 1;
};
Prim.debugPrint(output);
}

//SKIP run
//SKIP run-ir
//SKIP run-low
