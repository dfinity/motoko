import Prim "mo:prim";
import Employees "Employees";

persistent actor a {
  let employees = do {
    let es = Employees.Employees();
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
