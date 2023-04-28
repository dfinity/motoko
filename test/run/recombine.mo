let foo : {#foo : Nat; #bar : Char} = #foo 42;

switch foo {
  case (#foo n) { #foo n };
  case o o
}
