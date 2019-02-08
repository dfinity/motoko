let _ =
async {

{ var i = 0;
  var j = 0;
  label l
  loop {
   printInt(j);
   assert(j == i);
   i += 1;
   j += 1;
   if (j == 11) break l else continue l;
  } ;
  assert(i == 11);
};


{
  var i = 0;
  var j = 0;
  label l
  loop {
    printInt(j);
    assert(j == i);
    i += 1;
    j += 1;
    if (await async (j == 11)) break l else continue l;
    assert(false);
  };
  assert(i == 11);
};

{
  var i = 0;
  var j = 0;
  label l
  loop {
   printInt(j);
   assert(j == i);
   await (async (i += 1));
   j += 1;
   if (j == 11) break l else continue l;
   assert(false);
  };
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
  };
  assert(i == 11);
};

};





