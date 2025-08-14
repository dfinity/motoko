import Prim "mo:prim";
import Set "lib1/set";

actor Ok {

  transient let ver : Int = 0;

  // stable func dec
  stable func cmp(i : Int, j : Int) : Set.Order {
     Prim.debugPrint(debug_show (#cmp{ver; i; j}));
     if (i < j) #less else if (i == j) #equal else #greater
  };
  type Cmp = stable cmp (Int, Int) -> Set.Order; // singleton stable func type

  stable var s1 : Set.Set<Int, Cmp> = Set.empty(cmp);
  s1 := Set.add(s1, ver);

};
