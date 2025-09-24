import Prim "mo:prim";
import Employee "Employee";
import Employees "Employees";
import HashMap "HashMap";

persistent actor a {

  stable func natHash(n : Nat) : Nat { HashMap.natHash(n) };
  stable func natEqual(n1 : Nat, n2 : Nat) : Bool { HashMap.natEqual(n1, n2) };
  let employees = do {
    let es = Employees.new((natHash, natEqual));
    let raymond = es.add("Raymond"); // 1
    let alex = es.add("Alex"); // 2
    let christoph = es.add("Christoph"); // 3
    let luc = es.add("Luc"); // 4

    christoph.setManager(alex);
    alex.setManager(raymond);
    es
  };

  var id = 2;
  public func display() {
    let employee = employees.get(id);
    id += 1;
    switch (employee.getManager()) {
      case null Prim.debugPrint(employee.toText());
      case (?manager) Prim.debugPrint(employee.toText() # " <- " # manager.toText());
    }
  }
};
