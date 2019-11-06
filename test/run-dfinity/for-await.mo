let _ =
future {

{
  var i = 0;
  i := 0;
  for (j in range(0, 10)) {
   printNat(j);
   assert(j == i);
   await (future (i += 1));
  };
  assert(i == 11);
};

};
