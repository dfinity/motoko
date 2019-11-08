func printLn(x : Text) { debugPrint(x # "\n"); };
printLn(debug_show (true));
printLn(debug_show (false));
printLn(debug_show (-42000000000000));
printLn(debug_show (-42000000));
printLn(debug_show (-42000));
printLn(debug_show (-42));
printLn(debug_show (-0));
printLn(debug_show (+0));
printLn(debug_show (+42));
printLn(debug_show (+42000));
printLn(debug_show (+42000000));
printLn(debug_show (+420000000000000));
printLn(debug_show (0));
printLn(debug_show (42));
printLn(debug_show (42000));
printLn(debug_show (42000000));
printLn(debug_show (420000000000000));
printLn(debug_show (42,-42,()));
printLn(debug_show ("Foobar", null, null, ?23));
printLn(debug_show ([]));
printLn(debug_show ([1,2,3]));
printLn(debug_show ([var]));
printLn(debug_show ([var 1,2,3]));
class Foo() { public let foo : Int = 42; public var bar : Bool = true ; let hidden = [1,2] };
printLn(debug_show (Foo()));
printLn(debug_show (#foo ()));
printLn(debug_show (#bar (1, 2)));
printLn(debug_show (#bar {}));
printLn(debug_show (#bar ([])));
printLn(debug_show (#bar 42));
printLn(debug_show (#bar (-42)));
printLn(debug_show (#foo 42 : {#foo : Int; #bar : Text}));
printLn(debug_show (42 : Word16));
printLn(debug_show (42 : Nat8));
printLn(debug_show (42 : Int32));
printLn(debug_show (intToInt64 (-42)));
