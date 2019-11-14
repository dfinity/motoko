let a = [0, 1, 2, 3, 4];
let b = [];

ignore(async {
  ignore(a[5]);
  Debug.print("Unreachable code reached\n");
});
ignore(async {
  ignore(b[0]);
  Debug.print("Unreachable code reached\n");
});
