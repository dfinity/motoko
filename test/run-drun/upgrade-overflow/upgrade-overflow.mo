// fails to upgrade due to recursive implementation of Candid serialization
actor {

   type List = ?(Int, List);

   func list(n : Int, acc : List) : List {
       if (n == 0) acc
       else list(n - 1, ?(n,acc));
   };

   stable var l = list(10000, null);

}
