// test some forms of 2nd class continuations

let a0 = label c : Nat { loop { break c 42 }; 25 };
let b0 = label c : Nat { loop { break c 42 } while true; 25 };
let c0 = label c : Nat { while true { break c 42 }; 25 };
let d0 = label c : Nat { for (_ in "HEY!".chars()) { break c 42 }; 25 };
let e0 = label c : Nat { break c 42; 25 };

let a1 = label c : Nat loop { break c 42 };
let b1 = label c : Nat loop { break c 42 } while true;
let c1 = label c : Nat while true { break c 42 };
let d1 = label c : Nat for (_ in "HEY!".chars()) { break c 42 };
let e1 = label c : Nat break c 42;
