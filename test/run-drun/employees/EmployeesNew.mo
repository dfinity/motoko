import HashMap "HashMap";
import Prim "mo:prim";
import Employee "EmployeeNew";

module {
  public persistent class Employees() {
    var currentId = 0;
    let employees = HashMap.SimpleHashMap<Nat, Employee.Employee>(10, HashMap.natEqual, HashMap.natHash);

    public func add(name : Text) : Employee.Employee {
      currentId += 1;
      let e = Employee.Employee(currentId, name);
      employees.put(currentId, e);
      e
    };

    public func get(id : Nat) : Employee.Employee {
      let ?e = employees.get(id) else Prim.trap("Unknown employee");
      e
    };

  };
}
