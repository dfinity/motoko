let _ = 255 : Nat8;
let _ = 65535 : Nat16;
let _ = 4_294_967_295 : Nat32;
let _ = 18_446_744_073_709_551_615 : Nat64;


let _ = 127 : Int8;
let _ = 32767 : Int16;
let _ = 2_147_483_647 : Int32;
let _ = 9_223_372_036_854_775_807 : Int64;


// TODO(gabor) below limits are off-by one, as explained
//             in the open issue in #487.
let _ = -127 : Int8;
let _ = -32767 : Int16;
let _ = -2_147_483_647 : Int32;
let _ = -9_223_372_036_854_775_807 : Int64;
