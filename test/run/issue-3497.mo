import { debugPrint } = "mo:⛔";

let t = [var (0x00, 0x00)];

for ((k, v) in t.vals()) {
    debugPrint(debug_show(k, v));
}