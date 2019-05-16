// print a table of numbers (somewhat) quickly.
// we correlate these numbers with times that we
// measure elsewhere, where these numbers are not available.

let T = (import "../serverTypes.as");
let Model = (import "../serverModel.as");

let m = Model.Model();

let scales = [1,2,3,4,5,6,7,8,9,10,
              20,50,100];

print "# column: region count\n";
print "# column: workload scale\n";
print "# column: inventory count\n";
print "# column: route count\n";

for (scale in scales.vals()) {
  let (ic, rc) = m.countAddReqs(3, 1,
      5 * scale,
      5 * scale,
      5 * scale,
      5);

  printInt 5;
  print ", ";
  printInt scale;
  print ", ";
  printInt ic;
  print ", ";
  printInt rc;
  print "\n";
};
