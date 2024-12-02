import { floatCopySign; floatToInt64 } = "mo:⛔";

assert 0 == floatToInt64(-0.5);
assert 0 == floatToInt64(floatCopySign(0.0, -1.0));
