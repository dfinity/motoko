let Array_thaw = func <A>(xs : A[]) : var A[] {
  let xsLen = xs.len();
  if (xsLen == 0) {
    return [];
  };
  let ys = Array_init<A>(xsLen, xs[0]);
  for (i in ys.keys()) {
    ys[i] := xs[i]; // Comment out this line to prevent the error
  };
  ys;
};

(func () {
  let xs : Int[] = [ 1, 2, 3 ];

  let actual = Array_thaw<Int>(xs);
  let expected : var Int[] = [ 1, 2, 3 ];

  assert(actual.len() == expected.len());
 
  for (i in actual.keys()) {
    assert(actual[i] == expected[i]);
  };
})();
