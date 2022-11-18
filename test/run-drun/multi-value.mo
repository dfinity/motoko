func foo(b : Bool, n : Int) : (Int, Int) {
  if (b) {
    (n,1)
  } else {
    (n,2)
  }
};
ignore(foo(true,5));
