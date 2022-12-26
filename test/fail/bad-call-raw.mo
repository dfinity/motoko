import P "mo:⛔";

actor self {
  let a = P.call_raw("","foo",""); // reject, send capability required
};

