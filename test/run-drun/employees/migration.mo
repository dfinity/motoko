import Employee "EmployeeNew";
import Employees "EmployeesNew";
import Prim "mo:prim";

module {

  type OldEmployee = {
    id : Nat;
    name : Text;
    setManager : (newManager : OldEmployee) -> ();
    getManager : () -> ?OldEmployee;
    toText : () -> Text;
  };

  type NewEmployee = Employee.Employee;

  type OldEmployees = {
    add : persistent (name : Text) -> OldEmployee;
    addExisting : persistent (employee : OldEmployee) -> ();
    get : persistent (id : Nat) -> OldEmployee;
    entries : persistent () -> [(Nat, OldEmployee)];
  };

  type OldActor = {
    employees : OldEmployees;
  };

  type NewActor = {
    employees : Employees.Employees;
  };

  public func run(old : OldActor) : NewActor {
    let newEmployees = Employees.Employees();
    for ((_, oldEmployee) in old.employees.entries().values()) {
      let e = Employee.Employee(oldEmployee.id, oldEmployee.name);
      switch (e.getManager()) {
        case null {};
        case (?m) e.setManager(m);
      };
      newEmployees.addExisting(e)
    };

    { employees = newEmployees }
  };
};
