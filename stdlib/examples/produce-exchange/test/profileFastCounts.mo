// debugPrint a table of numbers (somewhat) quickly.
// we correlate these numbers with times that we
// measure elsewhere, where these numbers are not available.

import T = "../serverTypes.mo";
import Model = "../serverModel.mo";

let m = Model.Model();

let scales = [1,2,3,4,5,6,7,8,9,10,
              20,50,100];

debugPrint "# column: region count\n";
debugPrint "# column: workload scale\n";
debugPrint "# column: inventory count\n";
debugPrint "# column: route count\n";

for (scale in scales.vals()) {
  let (ic, rc) = m.countAddReqs(3, 1,
      5 * scale,
      5 * scale,
      5 * scale,
      5);

  debugPrintInt 5;
  debugPrint ", ";
  debugPrintInt scale;
  debugPrint ", ";
  debugPrintInt ic;
  debugPrint ", ";
  debugPrintInt rc;
  debugPrint "\n";
};
