let _ =
async {

{ var i = 0;
  var j = 0;
  while (j <= 10) {
   printInt(j);
   assert(j == i);
   i += 1;
   j += 1;
  };
  assert(i == 11);
};


{
  var i = 0;
  var j = 0;
  while (await async (j <= 10)) {
    printInt(j);
    assert(j == i);
    i += 1;
    j += 1;
  };
  assert(i == 11);
};

{
  var i = 0;
  var j = 0;
  while (j <= 10) {
   printInt(j);
   assert(j == i);
   await (async (i += 1));
   j += 1;
  };
  assert(i == 11);
};

{
  var i = 0;
  var j = 0;
  label l
  while (true) {
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
  };
  assert(i == 11);
};

};
