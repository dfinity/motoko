let _ =
async {

{ var i = 0;
  var j = 0;
  loop {
   debugPrintNat(j);
   assert(j == i);
   i += 1;
   j += 1;
  } while (i < 3);
  assert(i == 3);
};


{
  var i = 0;
  var j = 0;
  loop {
    debugPrintNat(j);
    assert(j == i);
    i += 1;
    j += 1;
  } while (await async (i < 3));
  assert(i == 3);
};

{
  var i = 0;
  var j = 0;
  loop {
   debugPrintNat(j);
   assert(j == i);
   await (async (i += 1));
   j += 1;
  } while (i < 3);
  assert(i == 3);
};

{
  var i = 0;
  var j = 0;
  label l
  loop {
   if (j > 2) {
     break l;
     assert(false);
   };
   debugPrintNat(j);
   assert(j == i);
   await (async (i += 1));
   j += 1;
   continue l;
   assert(false);
  } while (true);
  assert(i == 3);
};

};
