let _ =
async {

{ var i = 0;
  var j = 0;
  while (j <= 2) {
   Debug.printNat(j);
   assert(j == i);
   i += 1;
   j += 1;
  };
  assert(i == 3);
};


{
  var i = 0;
  var j = 0;
  while (await async (j <= 2)) {
    Debug.printNat(j);
    assert(j == i);
    i += 1;
    j += 1;
  };
  assert(i == 3);
};

{
  var i = 0;
  var j = 0;
  while (j <= 2) {
   Debug.printNat(j);
   assert(j == i);
   await (async (i += 1));
   j += 1;
  };
  assert(i == 3);
};

{
  var i = 0;
  var j = 0;
  label l
  while (true) {
   if (j > 2) {
     break l;
     assert(false);
   };
   Debug.printNat(j);
   assert(j == i);
   await (async (i += 1));
   j += 1;
   continue l;
   assert(false);
  };
  assert(i == 3);
};

};
