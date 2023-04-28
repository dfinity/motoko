var foo : {#foo : Nat; #bar : Char} = #foo (40 + 2);

func matchX(foo : {#foo : Nat; #bar : Char}) : {#foo : Nat; #bar : Char} = switch foo {
  case (#foo n) { #foo n };
  case o o
};

ignore matchX(foo)

// CHECK: func $matchX
// CHECK: local.get $switch_in
// CHECK: local.tee $tag_scrut
// CHECK: i32.const 5097222
// CHECK: local.get $foo
// CHECK-NEXT: br 1 (;@1;)
// CHECK: local.get $foo
// CHECK-NEXT: br 1 (;@1;)
