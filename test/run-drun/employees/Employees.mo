import HashMap "HashMap";
import Prim "mo:prim";
import Employee "Employee";

module {
  public persistent class Employees() {
    public var currentId = 0;
    let employees = HashMap.SimpleHashMap<Nat, Employee.Employee>(10, HashMap.natEqual, HashMap.natHash);

    public func add(name : Text) : Employee.Employee {
      currentId += 1;
      let e = Employee.Employee(currentId, name);
      employees.put(currentId, e);
      e
    };

    public func addExisting(employee : Employee.Employee) {
      employees.put(employee.id, employee)
    };

    public func get(id : Nat) : Employee.Employee {
      let ?e = employees.get(id) else Prim.trap("Unknown employee");
      e
    };

    public func entries() : [(Nat, Employee.Employee)] {
      employees.entries()
    };

  };
}
