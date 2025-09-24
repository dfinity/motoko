import Employee "EmployeeNew";
import Employees "EmployeesNew";
import OldEmployees "Employees";
import Prim "mo:prim";

module {

  type OldActor = {
    employees : OldEmployees.Employees;
  };

  type NewActor = {
    employees : Employees.Employees;
  };

  public func run(old : OldActor) : NewActor {
    // Bug: Required to make upgrade work
    let oldFake = OldEmployees.Employees();

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
