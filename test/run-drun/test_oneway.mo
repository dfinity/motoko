import M "oneway.mo";

ignore (async {
  debugPrint("A");
  M.Oneway.oneway();
  debugPrint("B");
  M.Oneway.onewayAlt();
  debugPrint("C");
  M.Oneway.discard();
  debugPrint("D");
  M.Oneway.discardAlt();
  debugPrint("E");
}
);

//SKIP comp