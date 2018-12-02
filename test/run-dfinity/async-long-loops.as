/* test asynchronous loops don't blow the stack,
   ensured by optimization of self-tail calls
*/

let _ =
{
  var done = false;
  var i = 0;
  while (i < 1000) {
    if (i == 500)
       done := true;
    i += 1;
  };
  assert(done);
  assert(i == 1000);

  print ("synchronus loop done\n");
};

let _ =
async {

  var done = false;
  var i = 0;
  while (i < 1000) {
    if (i == 500)
       await async  done := true;
    i += 1;
  };
  assert(done);
  assert(i == 1000);

  print ("asynchronus loop done\n");

};





