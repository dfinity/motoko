import Prim "mo:prim";
import Employee "EmployeeNew";
import HashMap "HashMap";

module {
  public type Self<H <: HashMap.HashFn<Nat>> = {
    var currentId : Nat;
    employees : HashMap.HashMap<Nat, Employee.Employee, H>;
  };

  public type Employees = Self<HashMap.HashFn<Nat>>;

  public func new<H <: HashMap.HashFn<Nat>>(hashFn : H) : Self<H> {
    { var currentId = 0;
      employees = HashMap.empty(10, hashFn);
    }
  };

  public func add(es : Employees, name : Text) : Employee.Employee {
    es.currentId += 1;
    let e = Employee.new(es.currentId, name);
    es.employees.put(es.currentId, e);
    e
  };

  public func addExisting(es : Employees, employee : Employee.Employee) {
    es.employees.put(employee.id, employee)
  };

  public func get(es : Employees, id : Nat) : Employee.Employee {
    let ?e = es.employees.get(id) else Prim.trap("Unknown employee");
    e
  };

  public func entries(es : Employees) : [(Nat, Employee.Employee)] {
    es.employees.entries()
  };
}
