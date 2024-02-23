// test small scalar ops
import { Array_tabulate; performanceCounter; debugPrint; rts_heap_size } = "mo:⛔";

actor Tagged {

  func counters() : (Int, Nat64) = (rts_heap_size(), performanceCounter(0));

  public func go() : async () {
    var i = 0;
    let (m0, n0) = counters();
    while (i < 16) {
      var n : Nat16 = 0;

      var temp : Nat16 = 0;
      while (n < 65535) {
        temp := n +% n;
        temp := n -% n;
        temp := ^n;
        temp := n *% n;
        n += 1;
      };
      i += 1;
    };
    let (m1, n1) = counters();
    debugPrint(debug_show (m1 - m0, n1 - n0));
  }
}

//CALL ingress go 0x4449444C0000
