import { debugPrint } = "mo:⛔";

do {
  let t = [var (0x00, 0x00)];

  for ((k, v) in t.vals()) {
    debugPrint(debug_show(k, v));
  }
};

do {
  let t = [(0x01, 0x01)];

  for ((k, v) in t.vals()) {
    debugPrint(debug_show(k, v));
  }
};
