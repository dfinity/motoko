func printLn(x : Text) { print(x # "\n"); };
printLn(show<Bool>(true));
printLn(show<Bool>(false));
printLn(show<Int>(-42));
printLn(show<Nat>(42));
printLn(show<(Nat,Int,())>(42,-42,()));
printLn(show<(Text,Null,?Int, ?Int)>("Foobar", null, null, ?23));
printLn(show<[Int]>([1,2,3]));
printLn(show<[var Int]>([var 1,2,3]));
