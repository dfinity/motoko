func specials(one : { #one }, two : { #c0; #c1 }) {
  var count = 0;
  switch one {
    case (#one) count += 1;
  };

  switch two {
    case (#c0) count += 1;
    case (#c1) count += 2;
  };

  count += 123;
};


specials(#one, #c1)

//CHECK: func $specials
//CHECK: i32.const 5544550
//CHECK: local.set $count

//CHECK: i32.const 22125
//CHECK: local.set $count

//CHECK: i32.const 22126
//CHECK: local.set $count

//CHECK: i32.const 246
