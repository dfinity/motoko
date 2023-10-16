actor {

/*
  type t table: type table0 = record { 48 : table1; 65 : empty }
       type table1 = opt table0
       wire_type: table0
*/

  type table0 = { H : table1; e : None};
  type table1 = ?table0;

  public func go(t : table0) : async () {
  }


}

//SKIP run
//SKIP run-low
//SKIP run-ir
//CALL ingress go 0x4449444c026c023001416f6e0002000001010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010100
