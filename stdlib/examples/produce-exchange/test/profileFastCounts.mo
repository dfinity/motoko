// debug_print a table of numbers (somewhat) quickly.
// we correlate these numbers with times that we
// measure elsewhere, where these numbers are not available.

import T = "../serverTypes.mo";
import Model = "../serverModel.mo";

let m = Model.Model();

let scales = [1,2,3,4,5,6,7,8,9,10,
              20,50,100];

debug_print "# column: region count\n";
debug_print "# column: workload scale\n";
debug_print "# column: inventory count\n";
debug_print "# column: route count\n";

for (scale in scales.vals()) {
  let (ic, rc) = m.countAddReqs(3, 1,
      5 * scale,
      5 * scale,
      5 * scale,
      5);

  debug_print_Int 5;
  debug_print ", ";
  debug_print_Int scale;
  debug_print ", ";
  debug_print_Int ic;
  debug_print ", ";
  debug_print_Int rc;
  debug_print "\n";
};
