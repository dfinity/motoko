ignore async {
  var x = "A";
  debugPrint x;
  let a = async {
    debugPrint "Now in async";
    debugPrint x;
    x := "B";
    debugPrint x;
  };
  debugPrint x;
  x := "C";
  debugPrint x;
  await a;
  debugPrint x;
};
