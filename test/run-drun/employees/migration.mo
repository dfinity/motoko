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
    newEmployees.currentId := old.employees.currentId;
    for ((_, oldEmployee) in old.employees.entries().values()) {
      let e = Employee.Employee(oldEmployee.id, oldEmployee.name);
      newEmployees.addExisting(e)
    };

    for ((_, oldEmployee) in old.employees.entries().values()) {
      let e = newEmployees.get(oldEmployee.id);
      switch (oldEmployee.getManager()) {
        case null {};
        case (?m) { e.setManager(newEmployees.get(m.id)) };
      };
    };

    { employees = newEmployees }
  };
};
