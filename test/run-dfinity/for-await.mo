let _ =
async {

{
  var i = 0;
  i := 0;
  for (j in range(0, 10)) {
   debug_print_Nat(j);
   assert(j == i);
   await (async (i += 1));
  };
  assert(i == 11);
};

};
