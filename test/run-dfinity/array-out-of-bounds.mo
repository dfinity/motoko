let a = [0, 1, 2, 3, 4];
let b = [];

ignore(future {
  ignore(a[5]);
  print("Unreachable code reached\n");
});
ignore(future {
  ignore(b[0]);
  print("Unreachable code reached\n");
});
