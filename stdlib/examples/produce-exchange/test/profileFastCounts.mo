// Debug.print a table of numbers (somewhat) quickly.
// we correlate these numbers with times that we
// measure elsewhere, where these numbers are not available.

import T = "../serverTypes.mo";
import Model = "../serverModel.mo";

let m = Model.Model();

let scales = [1,2,3,4,5,6,7,8,9,10,
              20,50,100];

Debug.print "# column: region count\n";
Debug.print "# column: workload scale\n";
Debug.print "# column: inventory count\n";
Debug.print "# column: route count\n";

for (scale in scales.vals()) {
  let (ic, rc) = m.countAddReqs(3, 1,
      5 * scale,
      5 * scale,
      5 * scale,
      5);

  Debug.printInt 5;
  Debug.print ", ";
  Debug.printInt scale;
  Debug.print ", ";
  Debug.printInt ic;
  Debug.print ", ";
  Debug.printInt rc;
  Debug.print "\n";
};
