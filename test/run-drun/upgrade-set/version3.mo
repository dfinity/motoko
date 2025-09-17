import Prim "mo:prim";
import Set "lib1/set";

actor Ok {

  transient let ver = 3;

  // stable func dec
  stable func cmp(i : Int, j : Int) : Set.Order {
     Prim.debugPrint(debug_show (#cmp{ver; i; j}));
     if (i < j) #less else if (i == j) #equal else #greater
  };

  stable var s1 : Set.Set<Int, cmp> = Prim.trap("oops");
  s1 := s1.add(ver);


};
