// print a table of numbers (somewhat) quickly.
// we correlate these numbers with times that we
// measure elsewhere, where these numbers are not available.

import Prim "mo:prim";
import T = "../serverTypes.mo";
import Model = "../serverModel.mo";

let m = Model.Model();

let scales = [1,2,3,4,5,6,7,8,9,10,
              20,50,100];

Prim.debugPrint "# column: region count\n";
Prim.debugPrint "# column: workload scale\n";
Prim.debugPrint "# column: inventory count\n";
Prim.debugPrint "# column: route count\n";

for (scale in scales.vals()) {
  let (ic, rc) = m.countAddReqs(3, 1,
      5 * scale,
      5 * scale,
      5 * scale,
      5);

  Prim.debugPrintInt 5;
  Prim.debugPrint ", ";
  Prim.debugPrintInt scale;
  Prim.debugPrint ", ";
  Prim.debugPrintInt ic;
  Prim.debugPrint ", ";
  Prim.debugPrintInt rc;
  Prim.debugPrint "\n";
};
