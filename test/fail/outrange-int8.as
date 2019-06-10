let _ = intToInt8 127;
let _ = intToInt8 (-128); // bug: should be (-128)


let _ = intToInt16 32767;
let _ = intToInt16 (-32768); // bug: should be (-32768)

let _ = intToInt32 2147483647;
let _ = intToInt32 (-2147483648);
