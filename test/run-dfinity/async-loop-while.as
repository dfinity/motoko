ignore(async {

{ var i = 0;
  var j = 0;
  loop {
   printInt(j);
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
    printInt(j);
    assert(j == i);
    i += 1;
    j += 1;
  } while (await async (i < 11));
  assert(i == 11);
};

{
  var i = 0;
  var j = 0;
  loop {
   printInt(j);
   assert(j == i);
   await (async (i += 1));
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
   printInt(j);
   assert(j == i);
   await (async (i += 1));
   j += 1;
   continue l;
   assert(false);
  } while (true);
  assert(i == 11);
};

});
