import E "../stdlib/env.mo";
import T "../stdlib/table.mo";
import Array "../stdlib/array.mo";
import P "../stdlib/prelude.mo";
import N "../stdlib/nat.mo";

type ReceiverId = Nat;
type Allocation = (ReceiverId, Nat); 


class CapTable(env : E.Env) {
  let ct = T.Table<Allocation, Bool>(env, (0,0), false);

  // Allocate tokens to receiver. Return allocation id.
  public func paper(receiver : ReceiverId, amount : Nat) : Nat {
    ct.append((receiver,amount),true);
    ct.len()-1;
  };

  public func list() : [Allocation] {
    ct.eval(func(a, b){if b [a] else []});
  }; 

  public func listall() : [(Allocation, Bool)] {
    ct.export();
  }; 

  public func revoke(allocId : Nat) {
    ct.write1(allocId, false);
  };

  public func enable(allocId : Nat) {
    ct.write1(allocId, true);
  };

  public func sum(receiver : ReceiverId) : Nat {
    func sum(l : [Nat]) : Nat = Array.foldr<Nat,Nat>(N.add, 0, l);

    sum( ct.evalC<Nat>(func(a,b){if (b and a.0 == receiver) [a.1] else []}) );
  };
};

let env = E.Env();

//let ct = CapTable(env, (1,1000), true);
let ct = CapTable(env);

{
  P.printLn("allocate some tokens to receivers 1 and 2");
  ignore( ct.paper(1,1000) ); // returns allocation id 1
  ignore( ct.paper(2,5000) ); // returns allocation id 2

  P.printLn("allocate some more tokens to receivers 1"); 
  ignore( ct.paper(1,200) ); // returns allocation id 3

  P.printLn("list allocations");
  P.printLn(debug_show( ct.list() ));

  P.printLn("revoke allocation id 1");
  ignore( ct.revoke(1) );

  P.printLn("list() only shows the non-revoked allocations");
  P.printLn(debug_show( ct.list() ));

  P.printLn("listall() shows all allocations");
  P.printLn("TODO remove the init allocation (0,0) with allocation id 0");
  P.printLn(debug_show( ct.listall() ));

  P.printLn("re-enable allocation id 1");
  ignore( ct.enable(1) );

  P.printLn("list allocations");
  P.printLn(debug_show( ct.list() ));

  P.printLn("sum of all active allocations for receiver 1");
  P.printLn(debug_show( ct.sum(1) ));

  P.printLn("revoke allocation id 1 again");
  ignore( ct.revoke(1) );

  P.printLn("sum of all active allocations for receiver 1");
  P.printLn(debug_show( ct.sum(1) ));
};

