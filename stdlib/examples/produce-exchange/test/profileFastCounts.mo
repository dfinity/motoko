// print a table of numbers (somewhat) quickly.
// we correlate these numbers with times that we
// measure elsewhere, where these numbers are not available.

import Debug "mo:stdlib/Debug";
import T "../src/serverTypes";
import Model "../src/serverModel";

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

  Debug.print (debug_show 5 # ", " # debug_show scale # ", " # debug_show ic # ", " # debug_show rc # "\n");
};
