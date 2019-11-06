let _ =
future {

{ var i = 0;
  var j = 0;
  loop {
   printNat(j);
   assert(j == i);
   i += 1;
   j += 1;
  } while (i < 11);
  assert(i == 11);
};


{
  var i = 0;
  var j = 0;
  loop {
    printNat(j);
    assert(j == i);
    i += 1;
    j += 1;
  } while (await future (i < 11));
  assert(i == 11);
};

{
  var i = 0;
  var j = 0;
  loop {
   printNat(j);
   assert(j == i);
   await (future (i += 1));
   j += 1;
  } while (i < 11);
  assert(i == 11);
};

{
  var i = 0;
  var j = 0;
  label l
  loop {
   if (j > 10) {
     break l;
     assert(false);
   };
   printNat(j);
   assert(j == i);
   await (future (i += 1));
   j += 1;
   continue l;
   assert(false);
  } while (true);
  assert(i == 11);
};

};
